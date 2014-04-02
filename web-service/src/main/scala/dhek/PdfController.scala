package dhek

import java.io.{ File, FileInputStream, FileReader, OutputStream, PrintWriter }
import javax.servlet.http.HttpServletResponse
import scala.concurrent.{ Await, Future }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Failure, Success }

import argonaut._, Argonaut._
import argonaut.{ Json ⇒ ArgJson }
import com.itextpdf.text.pdf.{
  PdfDictionary,
  PdfObject,
  PdfName,
  PdfReader,
  PdfStream,
  PdfWriter,
  PdfStamper
}
import com.itextpdf.text.{
  BaseColor,
  FontFactory
}
import reactivemongo.bson.BSONDocument
import resource.{ ManagedResource, managed }

object PdfController {

  case class Rect(x: Int, y: Int, h: Int, w: Int, name: String, typ: String)
  case class Page(areas: Option[List[Rect]])
  case class Model(format: String, pages: List[Page])

  // JSON codecs
  object Rect {
    implicit def rectCodecJson =
      casecodec6(Rect.apply, Rect.unapply)(
        "x", "y", "height", "width", "name", "type")
  }

  object Page {
    implicit def pageCodecJson =
      casecodec1(Page.apply, Page.unapply)("areas")
  }

  object Model {
    implicit def modelCodecJson =
      casecodec2(Model.apply, Model.unapply)("format", "pages")
  }

  object TemplateNotFound extends Exception
  object UserNotFound extends Exception
  case class ArgonautError(msg: String) extends Exception {
    override def toString = s"Argonaut error: $msg"
  }

  def merge(env: Env, appToken: String, templateId: String, hasParam: String ⇒ Boolean) {

    env async { complete ⇒
      env.withUsers.acquireAndGet { users ⇒

        def lookupTemplateInfo(user: BSONDocument): Future[(String, String)] = {
          val templates =
            user.getAs[List[BSONDocument]]("templates").getOrElse(Nil)

          val templateOpt = templates.find { t ⇒
            t.getAs[String]("id").filter(_ == templateId).isDefined
          }

          val infoOpt = for {
            t ← templateOpt
            jsonName ← t.getAs[String]("json")
            pdfName ← t.getAs[String]("pdf")
          } yield (jsonName, pdfName)

          infoOpt.fold(
            Future.failed[(String, String)](TemplateNotFound))(Future(_))
        }

        def loadModel(jsonName: String): Future[Model] = {
          val res =
            Parse.decodeEither[Model](
              Binaries.loadFile(s"${env.settings.repo}/$jsonName"))

          res.fold(e ⇒ Future.failed(ArgonautError(e)), Future(_))
        }

        val future: Future[(Model, String)] = for {
          uOpt ← users.find(BSONDocument("appToken" -> appToken)).one[BSONDocument]
          user ← uOpt.fold(Future.failed[BSONDocument](UserNotFound))(Future(_))
          (jsonName, pdfName) ← lookupTemplateInfo(user)
          model ← loadModel(jsonName)
        } yield (model, pdfName)

        Await.ready(future, env.settings.timeout) onComplete {
          case Success((model, pdfName)) ⇒
            val action = for {
              output ← env.outputStream
              reader ← managed(new PdfReader(s"${env.settings.repo}/$pdfName"))
              stamper ← managed(new PdfStamper(reader, output))
            } yield {
              def foreachObject[U](f: PdfObject ⇒ U) {
                val objCount = reader.getXrefSize

                @annotation.tailrec
                def loop(cur: Int) {
                  if (cur < objCount) {
                    f(reader.getPdfObject(cur - 1))
                    loop(cur + 1)
                  }
                }

                loop(1)
              }

              env.resp.setContentType("application/pdf")

              model.pages.foldLeft(1) { (i, p) ⇒
                val page = stamper.getImportedPage(reader, i)
                val pageRect = reader.getPageSize(i)
                val over = stamper.getOverContent(i)

                for {
                  rects ← p.areas.toList
                  rect ← rects if hasParam(rect.name)
                } yield {
                  over.saveState()
                  over.setLineWidth(2)
                  over.setColorStroke(BaseColor.RED)
                  over.rectangle(rect.x, pageRect.getHeight - rect.y - rect.h, rect.w, rect.h)
                  over.stroke()
                  over.restoreState()
                }

                i + 1
              }
            }

            action.acquireAndGet(identity)
            complete()
          case Failure(e) ⇒
            e match {
              case UserNotFound     ⇒ env.jsonError(403, "dhek_token not found")
              case TemplateNotFound ⇒ env.jsonError(400, "dhek_template not found")
              case ArgonautError(e) ⇒ env.jsonError(500, s"model json error: $e")
              case e ⇒
                e.printStackTrace()
                env.jsonError(500, e.getMessage)
            }

            complete()
        }
      }
    }
  }
}
