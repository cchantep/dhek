package dhek

import scala.util.{ Success, Failure }
import scala.concurrent.{ Await, Future }
import scala.concurrent.ExecutionContext.Implicits.global

import argonaut._, Argonaut._
import reactivemongo.bson.{ BSONDocument, BSONObjectID }
import resource.managed
import scalaz.std.list._
import scalaz.std.option._
import scalaz.std.scalaFuture._
import scalaz.syntax.traverse._

import Extractor.{ Param, Path, POST, & }

sealed trait Action
case class GetTemplates(token: String) extends Action
case class DeleteTemplates(token: String, tpids: List[String]) extends Action

object TemplateController {

  case class Template(id: String, name: String)

  object Template {
    implicit def templateCodecJson =
      casecodec2(Template.apply, Template.unapply)("id", "name")

    def fromBSON(b: BSONDocument): Option[Template] =
      for {
        id   <- b.getAs[String]("id")
        name <- b.getAs[String]("name")
      } yield Template(id, name)

    def toBSON(t: Template): BSONDocument =
      BSONDocument("id" -> t.id, "name" -> t.name)
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
              env.jsonError(500, e.getMessage)
              complete()
        }
      }
    }
    case DeleteTemplates(token, tpids) =>
      val collection = env.mongo.map { con =>
        val db = con("dhek")
        db("users")
      }

      env.async { complete =>
        collection.acquireAndGet { users =>

          def updateTemplates(user: BSONDocument): Future[Option[List[Template]]] = {
            def update(t: (List[Template], BSONObjectID)) = {
              val newTps     = t._1.filterNot(t => tpids.contains(t.id))
              val newTpsBSON = newTps.map(Template.toBSON)
              val modifier   = BSONDocument(
                "$set" -> BSONDocument(
                  "templates" -> newTpsBSON
                )
              )

              users.update(BSONDocument("_id" -> t._2), modifier).map(_ => newTps)
            }

            val action = for {
              bsons     <- user.getAs[List[BSONDocument]]("templates")
              id        <- user.getAs[BSONObjectID]("_id")
              templates <- bsons.traverse(Template.fromBSON)
            } yield (templates, id)

            action.traverse(update)
          }

          val deletion = for {
            user <- users.find(BSONDocument("adminToken" -> token)).one[BSONDocument]
            tps  <- user.traverse(updateTemplates)
          } yield tps.flatten

          env.resp.setContentType("application/json")
          val writer = managed(env.resp.getWriter)

          Await.ready(deletion, env.settings.timeout).onComplete {
            case Success(res) =>
              res match {
                case None      => writer.acquireAndGet(_.print("null"))
                case Some(tps) =>
                  writer.acquireAndGet(_.print(tps.asJson.nospaces))
              }
              complete()
            case Failure(e) =>
              e.printStackTrace()
              env.jsonError(500, e.getMessage)
              complete()
          }
        }
      }
  }
}
