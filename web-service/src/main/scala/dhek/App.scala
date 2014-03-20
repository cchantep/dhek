package dhek

import java.io.File
import javax.servlet.{
  Filter,
  FilterConfig,
  FilterChain,
  MultipartConfigElement,
  ServletRequest,
  ServletResponse
}
import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }

import Extractor.{ &, Attributes, GET, POST, Path }

object App extends Filter with App {
  def destroy() {}
  def init(config: FilterConfig) {}

  def doFilter(req: ServletRequest, resp: ServletResponse, chain: FilterChain) {
    val hreq = req.asInstanceOf[HttpServletRequest]
    val hresp = resp.asInstanceOf[HttpServletResponse]

    hresp.setCharacterEncoding("UTF-8")

    hreq match {
      case GET(Path("/token")) ⇒ getToken(hreq, hresp)
      case POST(Path("/upload")) & Attributes(Pdf(p) & Json(j)) ⇒
        modelFusion(p, j, hresp)
      case _ ⇒ chain.doFilter(req, resp)
    }
  }
}

trait App {
  import java.io.InputStreamReader

  import scala.concurrent.ExecutionContext.Implicits.global

  import argonaut._, Argonaut._
  import com.itextpdf.text.{ Document, Image, BaseColor }
  import com.itextpdf.text.pdf.{ PdfReader, PdfWriter, PdfStamper }
  import org.eclipse.jetty.continuation.ContinuationSupport
  import reactivemongo.api._
  import reactivemongo.bson._
  import resource.managed

  case class Rect(x: Int, y: Int, h: Int, w: Int, name: String, typ: String)
  case class Page(areas: Option[List[Rect]])
  case class Model(format: String, pages: List[Page])

  val driver = new MongoDriver

  object Pdf {
    def unapply(attrs: Map[String, Any]) =
      attrs.get("pdf").map(_.asInstanceOf[File])
  }

  object Json {
    def unapply(attrs: Map[String, Any]) =
      attrs.get("json").map(_.asInstanceOf[File])
  }

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

  private def onError(resp: HttpServletResponse)(e: String) {
    resp.setStatus(400)
    resp.getWriter.print(e)
  }

  def getToken(req: HttpServletRequest, resp: HttpServletResponse) {
    val connection = driver.connection(List("localhost"))
    val db = connection("dhek")
    val tokens = db("tokens")
    val query = BSONDocument("name" -> "public")
    val filter = BSONDocument("token" -> 1)
    val continuation = ContinuationSupport.getContinuation(req)

    continuation.suspend(resp)
    tokens.find(query, filter).one[BSONDocument].foreach { res ⇒
      for {
        doc ← res
        token ← doc.getAs[String]("token")
      } yield {
        val cresp = continuation.getServletResponse
        cresp.setContentType("application/json")
        cresp.getWriter.print(s"""{"token":"$token"}""")
        continuation.complete()
      }
    }
  }

  def modelFusion(pdf: File, json: File, resp: HttpServletResponse) {

    def onJsonSuccess(m: Model) {

      managed(new PdfReader(new java.io.FileInputStream(pdf))).acquireAndGet { reader ⇒
        managed(new PdfStamper(reader, resp.getOutputStream)).acquireAndGet { stamper ⇒

          resp.setContentType("application/pdf")

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
      fold(onError(resp), onJsonSuccess)
  }
}
