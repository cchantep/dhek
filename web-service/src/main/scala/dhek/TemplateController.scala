package dhek

import scala.util.{ Success, Failure }
import scala.concurrent.{ Await, Future }
import scala.concurrent.ExecutionContext.Implicits.global

import argonaut._, Argonaut._
import reactivemongo.api.MongoConnection
import reactivemongo.bson.BSONDocument
import resource.ManagedResource
import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.traverse._

import Extractor.Param

object TemplateController {

  sealed trait Action

  case class GetTemplates(
    token: String,
    async: Async,
    mongo: ManagedResource[MongoConnection],
    settings: Settings
  ) extends Action

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

  def extractGetTemplates(ps: Route.Params) = ps match {
    case ~(Param("t"), token) =>
      for {
        async    <- Route.getAsync
        mongo    <- Route.getMongo
        settings <- Route.getSettings
      } yield GetTemplates(token, async, mongo, settings)
    case _ => Route.failed[GetTemplates]
  }

  val route = for {
    _  <- Route.isPOST
    _  <- Route.hasURI("my-templates")
    ps <- Route.getParams
    a  <- extractGetTemplates(ps)
  } yield a

  def apply(action: Action): Unit = action match {
    case GetTemplates(token, async, mongo, settings) =>
      val findTemplates = mongo.map { con =>
        val db = con("dhek")
        db("users").find(BSONDocument("adminToken" -> token)).one[BSONDocument]
      }

      async { (complete, resp, writer) =>
        findTemplates.acquireAndGet { ft =>
          val find: Future[Option[BSONDocument]] =
            Await.ready(ft, settings.timeout)

          resp.setContentType("application/json")

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
              resp.setStatus(500)
              writer.acquireAndGet(_.print(e.getMessage))
              complete()
        }
      }
    }
  }
}
