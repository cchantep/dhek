package dhek

import java.io.File
import java.util.UUID
import scala.util.{ Success, Failure }
import scala.concurrent.{ Await, Future }
import scala.concurrent.ExecutionContext.Implicits.global

import argonaut._, Argonaut._
import reactivemongo.core.commands.LastError
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.{ BSONDocument, BSONObjectID }
import resource.{ ManagedResource, managed }
import scalaz.std.list._
import scalaz.std.option._
import scalaz.std.scalaFuture._
import scalaz.syntax.traverse._

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
    val findUser = env.mongo.map { con ⇒
      val db = con("dhek")
      db("users").find(BSONDocument("adminToken" -> token)).one[BSONDocument]
    }

    env async { complete ⇒
      findUser acquireAndGet { fu ⇒
        val find: Future[Option[BSONDocument]] =
          Await.ready(fu, env.settings.timeout)

        env.resp.setContentType("application/json")

        find onComplete {
          case Success(user) ⇒
            val templates: Option[List[Template]] = user.map(getTemplates)

            env.writer.acquireAndGet { w ⇒
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

  private def getUser(users: BSONCollection, token: String): Future[Option[BSONDocument]] = users.find(BSONDocument("adminToken" -> token)).one[BSONDocument]

  private def getTemplates(user: BSONDocument): List[Template] = {
    val templates = for {
      bsons ← user.getAs[List[BSONDocument]]("templates")
      tps ← bsons.traverse(Template.fromBSON)
    } yield tps

    templates.getOrElse(Nil)
  }

  private def updateTemplates(users: BSONCollection)(user: BSONDocument, tps: List[Template]): Future[Option[List[Template]]] =
    user.getAs[BSONObjectID]("_id") match {
      case Some(id) ⇒
        val change = BSONDocument("$set" ->
          BSONDocument("templates" -> tps.map(Template.toBSON)))

        // TODO: Check LastError
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

        Await.ready(deletion, env.settings.timeout) onComplete {
          case Success(tps) ⇒
            env.writer.acquireAndGet(_.print(tps.fold("null")(_.asJson.nospaces)))
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
    println(s"token = $token, name = $name") // TODO: Logging

    val collection = env.mongo.map { con ⇒
      val db = con("dhek")
      db("users")
    }

    env async { complete ⇒
      collection.acquireAndGet { users ⇒
        val res: Future[String] = for {
          u ← getUser(users, token)
          r ← u.fold[Future[String]](
            Future.failed(new Throwable(s"No user: $token"))) { usr ⇒
              pdf.file.renameTo(new File(env.settings.repo, pdf.filename))
              json.file.renameTo(new File(env.settings.repo, json.filename))

              val ts = usr.getAs[List[BSONDocument]]("templates").getOrElse(Nil)
              val id = UUID.randomUUID.toString
              val t = BSONDocument("id" -> id, "name" -> name,
                "pdf" -> pdf.filename, "json" -> json.filename)

              val change = BSONDocument("$set" ->
                BSONDocument("templates" -> (ts :+ t)))

              users.update(
                BSONDocument("adminToken" -> token), change) flatMap {
                  case LastError(true, _, _, _, _, 1, _) ⇒ Future(id)
                  case err ⇒
                    Future.failed(err.fillInStackTrace)
                }
            }
        } yield r

        env.resp.setContentType("text/plain")

        Await.ready(res, env.settings.timeout) onComplete {
          case Success(id) ⇒
            env.writer.acquireAndGet(_.print(s"OK:$id"))
            complete()

          case Failure(e) ⇒
            e.printStackTrace()
            env.writer.acquireAndGet(_.print(e.getMessage))
            complete()
        }
      }
    }
  }
}
