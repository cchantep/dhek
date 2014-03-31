package dhek

import java.io.File
import java.util.UUID
import scala.util.{ Success, Failure }
import scala.concurrent.{ Await, Future }
import scala.concurrent.ExecutionContext.Implicits.global

import argonaut._, Argonaut._
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.{ BSONDocument, BSONObjectID }
import resource.{ ManagedResource, managed }
import scalaz.std.list._
import scalaz.std.option._
import scalaz.std.scalaFuture._
import scalaz.syntax.traverse._

import Extractor.{ Param, Path, POST, & }

object TemplateController {

  case class Template(id: String, name: String, json: String, pdf: String)

  object Template {
    implicit def templateCodecJson =
      casecodec4(Template.apply, Template.unapply)("id", "name", "json", "pdf")

    def fromBSON(b: BSONDocument): Option[Template] =
      for {
        id ← b.getAs[String]("id")
        name ← b.getAs[String]("name")
        json ← b.getAs[String]("json")
        pdf ← b.getAs[String]("pdf")
      } yield Template(id, name, json, pdf)

    def toBSON(t: Template): BSONDocument =
      BSONDocument(
        "id" -> t.id,
        "name" -> t.name,
        "json" -> t.json,
        "pdf" -> t.pdf
      )
  }

  def myTemplates(env: Env)(token: String): Unit = {
    val findTemplates = env.mongo.map { con ⇒
      val db = con("dhek")
      db("users").find(BSONDocument("adminToken" -> token)).one[BSONDocument]
    }

    env async { complete ⇒
      findTemplates acquireAndGet { ft ⇒
        val find: Future[Option[BSONDocument]] =
          Await.ready(ft, env.settings.timeout)

        env.resp.setContentType("application/json")
        val writer = managed(env.resp.getWriter)

        find onComplete {
          case Success(user) ⇒
            val templates: Option[List[Template]] = user.map(getTemplates)

            writer.acquireAndGet { w ⇒
              w.print(templates.fold("null")(_.asJson.nospaces))
              complete()
            }
          case Failure(e) ⇒
            e.printStackTrace()
            env.jsonError(500, e.getMessage)
            complete()
        }
      }
    }
  }

  private def getUser(users: BSONCollection, token: String): Future[Option[BSONDocument]] =
    users.find(BSONDocument("adminToken" -> token)).one[BSONDocument]

  private def getTemplates(user: BSONDocument): List[Template] = {
    val action = for {
      bsons ← user.getAs[List[BSONDocument]]("templates")
      tps ← bsons.traverse(Template.fromBSON)
    } yield tps

    action.getOrElse(Nil)
  }

  private def addTemplate(users: BSONCollection, t: Template)(user: BSONDocument): Future[Unit] = {
    val ts = getTemplates(user)
    val newTs = (t :: ts).map(Template.toBSON)
    val res = user.getAs[BSONObjectID]("_id").traverse { id ⇒
      val modifier = BSONDocument("$set" -> BSONDocument("templates" -> newTs))

      users.update(BSONDocument("_id" -> id), modifier)
    }

    res.map(_ ⇒ ())
  }

  private def updateTemplates(users: BSONCollection)(user: BSONDocument, tps: List[Template]): Future[Option[List[Template]]] =
    user.getAs[BSONObjectID]("_id") match {
      case Some(id) ⇒
        val change = BSONDocument("$set" ->
          BSONDocument("templates" -> tps.map(Template.toBSON)))

        users.update(BSONDocument("_id" -> id), change).map(_ ⇒ Option(tps))

      case _ ⇒ Future(None)
    }

  def removeTemplates(env: Env)(token: String, tpids: List[String]): Unit = {
    val collection = env.mongo.map { con ⇒
      val db = con("dhek")
      db("users")
    }

    env async { complete ⇒
      collection.acquireAndGet { coll ⇒
        val deletion: Future[Option[List[Template]]] = for {
          u ← coll.find(BSONDocument("adminToken" -> token)).one[BSONDocument]
          up ← u.fold[Future[Option[List[Template]]]](Future(None)) { user ⇒
            val bs = user.getAs[List[BSONDocument]]("templates").getOrElse(Nil)
            val tps: List[Template] = bs.map(Template.fromBSON).flatten

            updateTemplates(coll)(
              user, tps.filterNot(t ⇒ tpids.contains(t.id)))
          }
        } yield up

        env.resp.setContentType("application/json")
        val writer = managed(env.resp.getWriter)

        Await.ready(deletion, env.settings.timeout) onComplete {
          case Success(tps) ⇒
            writer.acquireAndGet(_.print(tps.fold("null")(_.asJson.nospaces)))
            complete()
          case Failure(e) ⇒
            e.printStackTrace()
            env.jsonError(500, e.getMessage)
            complete()
        }
      }
    }
  }

  def saveTemplate(env: Env)(token: String, name: String, pdf: FileInfo, json: FileInfo) {
    val collection = env.mongo.map { con ⇒
      val db = con("dhek")
      db("users")
    }

    env async { complete ⇒
      collection.acquireAndGet { users ⇒
        val id = UUID.randomUUID.toString
        val newTemplate = Template(id, name, json.filename, pdf.filename)
        val future = for {
          user ← getUser(users, token)
          _ ← user.traverse(addTemplate(users, newTemplate))
        } yield ()

        Await.ready(future, env.settings.timeout) onComplete {
          case Success(_) ⇒
            pdf.file.acquireAndGet(
              Binaries.writeToFile(_, s"${env.settings.repo}/${pdf.filename}"))
            json.file.acquireAndGet(
              Binaries.writeToFile(_, s"${env.settings.repo}/${json.filename}"))

            env.resp.setContentType("text/plain")
            managed(env.resp.getWriter).acquireAndGet(_.print(s"OK:$id"))
            complete()
          case Failure(e) ⇒
            e.printStackTrace()
            env.jsonError(500, e.getMessage)
            complete()
        }
      }
    }
  }
}
