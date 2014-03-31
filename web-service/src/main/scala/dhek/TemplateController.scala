package dhek

import scala.util.{ Success, Failure }
import scala.concurrent.{ Await, Future }
import scala.concurrent.ExecutionContext.Implicits.global

import argonaut._, Argonaut._
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.{ BSONDocument, BSONObjectID }
import resource.managed
import scalaz.std.list._
import scalaz.std.option._
import scalaz.std.scalaFuture._
import scalaz.syntax.traverse._

import Extractor.{ Param, Path, POST, & }

object TemplateController {

  case class Template(id: String, name: String)

  object Template {
    implicit def templateCodecJson =
      casecodec2(Template.apply, Template.unapply)("id", "name")

    def fromBSON(b: BSONDocument): Option[Template] =
      for {
        id ← b.getAs[String]("id")
        name ← b.getAs[String]("name")
      } yield Template(id, name)

    def toBSON(t: Template): BSONDocument =
      BSONDocument("id" -> t.id, "name" -> t.name)
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
          case Success(res) ⇒
            val templates: Option[List[Template]] = for {
              u ← res
              bs ← u.getAs[List[BSONDocument]]("templates").orElse(Some(Nil))
              ts ← bs.traverse(Template.fromBSON)
            } yield ts

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
}
