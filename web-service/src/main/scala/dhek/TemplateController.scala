package dhek

import scala.util.{ Success, Failure }
import scala.concurrent.{ Await, Future }
import scala.concurrent.ExecutionContext.Implicits.global

import argonaut._, Argonaut._
import reactivemongo.api.MongoConnection
import reactivemongo.bson.BSONDocument
import resource.managed
import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.traverse._

import Extractor.{ Param, Path, POST, & }

object TemplateController {

  sealed trait Action
  case class GetTemplates(token: String) extends Action

  val routing = Routing(route, apply)

  case class Template(id: String, name: String)

  object Template {
    implicit def templateCodecJson =
      casecodec2(Template.apply, Template.unapply)("id", "name")

    def fromBSON(b: BSONDocument): Option[Template] =
      for {
        id   <- b.getAs[String]("id")
        name <- b.getAs[String]("name")
      } yield Template(id, name)
  }

  def route(env: Env): Option[Action] = env.req match {
    case POST(Path("/my-templates")) & ~(Param("t"), t) =>
      Some(GetTemplates(t))
    case _ => None
  }

  def apply(action: Action, env: Env): Unit = action match {
    case GetTemplates(token) =>
      val findTemplates = env.mongo.map { con =>
        val db = con("dhek")
        db("users").find(BSONDocument("adminToken" -> token)).one[BSONDocument]
      }

      env.async { complete =>
        findTemplates.acquireAndGet { ft =>
          val find: Future[Option[BSONDocument]] =
            Await.ready(ft, env.settings.timeout)

          env.resp.setContentType("application/json")
          val writer = managed(env.resp.getWriter)

          find.onComplete {
            case Success(res) =>
              val action = for {
                doc       <- res
                bsons     <- doc.getAs[List[BSONDocument]]("templates").orElse(Some(Nil))
                templates <- bsons.traverse(Template.fromBSON)
              } yield templates

              writer.acquireAndGet { w =>
                action.fold(w.print("null")) { ts =>
                  w.print(ts.asJson.nospaces)
                }

                complete()
              }
            case Failure(e) =>
              e.printStackTrace()
              env.resp.setStatus(500)
              writer.acquireAndGet(_.print(e.getMessage))
              complete()
        }
      }
    }
  }
}
