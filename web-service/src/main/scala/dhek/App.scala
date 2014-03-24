package dhek

import java.io.{ File, FileInputStream }
import javax.servlet.{
  Filter,
  FilterConfig,
  FilterChain,
  MultipartConfigElement,
  ServletRequest,
  ServletResponse
}
import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }

import reactivemongo.api.MongoConnection

import Extractor.{ &, Attributes, GET, POST, Path }

final class Plan(m: => MongoConnection) 
    extends Filter with Controllers {

  def mongo: MongoConnection = m

  def destroy() {}
  def init(config: FilterConfig) {}

  def doFilter(req: ServletRequest, resp: ServletResponse, chain: FilterChain) {
    val hreq = req.asInstanceOf[HttpServletRequest]
    val hresp = resp.asInstanceOf[HttpServletResponse]

    hresp.setCharacterEncoding("UTF-8")

    hreq match {
      case GET(Path("/token")) ⇒ auth(hreq, hresp)
      case POST(Path("/upload") & Attributes(Pdf(p) & Json(j) & Font(f))) ⇒
        modelFusion(p, j, f, hresp)
      case _ ⇒ chain.doFilter(req, resp)
    }
  }
}

sealed trait Controllers { self: Plan ⇒ // TODO: Separate each controller
  import java.io.InputStreamReader

  import scala.util.{ Failure, Success }
  import scala.concurrent.{ Await, Future }
  import scala.concurrent.duration.Duration
  import scala.concurrent.ExecutionContext.Implicits.global

  import argonaut._, Argonaut._
  import com.itextpdf.text.{ Document, Image, BaseColor }
  import com.itextpdf.text.pdf.{
    PdfDictionary,
    PdfObject,
    PdfName,
    PdfReader,
    PdfStream,
    PdfWriter,
    PdfStamper
  }
  import org.eclipse.jetty.continuation.{ Continuation, ContinuationSupport }
  import reactivemongo.api._
  import reactivemongo.bson._
  import resource.managed

  case class Rect(x: Int, y: Int, h: Int, w: Int, name: String, typ: String)
  case class Page(areas: Option[List[Rect]])
  case class Model(format: String, pages: List[Page])

  // Request attributes extractors
  object Pdf {
    def unapply(attrs: Map[String, Any]): Option[File] =
      attrs.get("pdf").map(_.asInstanceOf[File])
  }

  object Json {
    def unapply(attrs: Map[String, Any]) =
      attrs.get("json").map(_.asInstanceOf[File])
  }

  object Font {
    def unapply(attrs: Map[String, Any]): Option[Option[File]] =
      Some(attrs.get("font").map(_.asInstanceOf[File]))
  }

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

  // TODO: JSON Format in body {"exception":"message"}
  private def jsonError(resp: HttpServletResponse)(e: String) {
    resp.setStatus(400)
    resp.getWriter.print(e)
  }

  def auth(req: HttpServletRequest, resp: HttpServletResponse) {
    val findTok = managed(self.mongo) map { con =>
      val db = con("dhek")
      val query = BSONDocument("name" -> "public")
      val filter = BSONDocument("token" -> 1)
      db("tokens").find(query, filter).one[BSONDocument]
    }

    val continuation = {
      val cont = ContinuationSupport.getContinuation(req)
      cont.setTimeout(5000) // TODO: configurable
      cont.suspend(resp)
      cont
    }

    findTok acquireAndGet { f =>
      val find: Future[Option[BSONDocument]] = 
        scala.concurrent.Await.ready(f, Duration("5 s")/* TODO: Configurable */)

      val r = continuation.getServletResponse.asInstanceOf[HttpServletResponse]
      r.setContentType("application/json")

      val err = jsonError(r)(_)

      find onComplete {
        case Success(o) => 
          o.fold[Unit](err("No matching token"))(t =>
            managed(r.getWriter).acquireAndGet(_.print(s"""{"token":"TODO-SHA1-HMAC"}""")))
          continuation.complete()

        case Failure(e) =>
          e.printStackTrace() // TODO: logging
          err(s"Fails to check authentication: ${e.getMessage}")
          continuation.complete()
      } 
    }
  }

  def modelFusion(pdf: File, json: File, font: Option[File], resp: HttpServletResponse) {

    def onJsonSuccess(m: Model) {

      managed(new PdfReader(new java.io.FileInputStream(pdf))).acquireAndGet { reader ⇒
        managed(new PdfStamper(reader, resp.getOutputStream)).acquireAndGet { stamper ⇒

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

          resp.setContentType("application/pdf")

          font.foreach { ft ⇒
            val stream = new PdfStream(Binaries.loadRawBytes(new FileInputStream(ft)))

            foreachObject { obj ⇒
              if (obj.isDictionary) {
                val dict = obj.asInstanceOf[PdfDictionary]

                if (PdfName.FONTDESCRIPTOR == dict.get(PdfName.TYPE)) {
                  val ref = stamper.getWriter.addToBody(stream)

                  dict.put(PdfName.FONTFILE2, ref.getIndirectReference)
                }
              }
            }
          }

          m.pages.foldLeft(1) { (i, p) ⇒

            val page = stamper.getImportedPage(reader, i)
            val pageRect = reader.getPageSize(i)
            val over = stamper.getOverContent(i)

            for {
              rects ← p.areas.toList
              rect ← rects
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
      }
    }

    lazy val jsonReader = new java.io.FileReader(json)

    Parse.decodeEither[Model](Binaries.loadReader(jsonReader)).
      fold(jsonError(resp), onJsonSuccess)
  }
}
