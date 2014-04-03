package dhek

import java.io.{ File, FileInputStream, FileReader, OutputStream, PrintWriter }
import javax.servlet.http.HttpServletResponse
import scala.concurrent.{ Await, Future }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Failure, Success }

import argonaut._, Argonaut._
import argonaut.{ Json ⇒ ArgJson }
import com.itextpdf.text.pdf.{
  BaseFont,
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

  case class Model(format: String, pages: List[Page])
  case class Page(areas: Option[List[Area]])
  case class Area(x: Int, y: Int, h: Int, w: Int,
    name: String, typ: String, value: Option[String] /* required for radio */ )

  // JSON codecs
  object Area {
    implicit def rectCodecJson =
      casecodec7(Area.apply, Area.unapply)(
        "x", "y", "height", "width", "name", "type", "value")
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

  def merge(env: Env, appToken: String, templateId: String, getParam: String ⇒ Option[String]): Unit = env async { complete ⇒
    println(s"token = $appToken, templateId = $templateId") // TODO: logging

    env.withUsers.acquireAndGet { users ⇒

      def lookupTemplateInfo(user: BSONDocument): Future[(String, String)] = {
        val templates =
          user.getAs[List[BSONDocument]]("templates").getOrElse(Nil)

        val infoOpt = for {
          t ← templates.find(
            _.getAs[String]("id").filter(_ == templateId).isDefined)
          jsonName ← t.getAs[String]("json")
          pdfName ← t.getAs[String]("pdf")
        } yield (jsonName, pdfName)

        infoOpt.fold(
          Future.failed[(String, String)](TemplateNotFound))(Future(_))
      }

      @inline def loadModel(jsonName: String): Future[Model] =
        Future(Parse.decodeEither[Model](
          Binaries.loadFile(s"${env.settings.repo}/$jsonName"))).flatMap(
          _.fold(e ⇒ Future.failed(ArgonautError(e)), Future(_)))

      val future: Future[(Model, String)] = for {
        uOpt ← users.find(BSONDocument(
          "appToken" -> appToken)).one[BSONDocument]
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

            env.resp.setContentType("application/pdf")

            model.pages.foldLeft(1) { (i, p) ⇒
              val page = stamper.getImportedPage(reader, i)
              val pageRect = reader.getPageSize(i)
              val over = stamper.getOverContent(i)

              for {
                areas ← p.areas.toList
                area ← areas
              } yield {
                val param = getParam(area.name)

                @inline def matches(f: String ⇒ Boolean) = param.fold(false)(f)
                @inline def checked = matches { v ⇒
                  val lower = v.toLowerCase
                  lower == "yes" || lower == "y" || lower == "on"
                }
                @inline def selected =
                  matches(v ⇒ area.value.fold(false)(_ == v))

                val normalizedY = pageRect.getHeight - area.y - area.h
                over.saveState()
                area.typ match {
                  case "text" ⇒
                    val font = BaseFont.createFont()
                    over.beginText()
                    over.moveText(area.x, normalizedY)
                    over.setRGBColorFill(0, 0, 255)
                    over.setFontAndSize(font, 11)
                    over.showText(param getOrElse "")
                    over.endText()

                  case "checkbox" if checked ⇒
                    over.setLineWidth(2)
                    over.setColorStroke(BaseColor.RED)
                    over.moveTo(area.x, normalizedY)
                    over.lineTo(area.x + area.w, normalizedY + area.h)
                    over.stroke()
                    over.moveTo(area.x + area.w, normalizedY)
                    over.lineTo(area.x, normalizedY + area.h)
                    over.stroke()

                  case "radio" if selected ⇒
                    over.setLineWidth(2)
                    over.setColorStroke(BaseColor.RED)
                    over.moveTo(area.x, normalizedY)
                    over.lineTo(area.x + area.w, normalizedY + area.h)
                    over.stroke()
                    over.moveTo(area.x + area.w, normalizedY)
                    over.lineTo(area.x, normalizedY + area.h)
                    over.stroke()

                  case _ ⇒ // nothing to do
                }
                over.restoreState()
              }

              i + 1
            }
          }

          action.acquireAndGet(identity)
          complete()
        case Failure(e) ⇒
          e match {
            case UserNotFound ⇒
              env.jsonError(403, s"User not found: $appToken")
            case TemplateNotFound ⇒
              env.jsonError(400, s"Template not found: $templateId")
            case ArgonautError(e) ⇒ env.jsonError(500, s"Model json error: $e")
            case e ⇒
              e.printStackTrace()
              env.jsonError(500, e.getMessage)
          }

          complete()
      }
    }
  }
}
