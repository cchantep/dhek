package dhek

import javax.servlet.{
  Filter,
  FilterConfig,
  FilterChain,
  MultipartConfigElement,
  ServletRequest,
  ServletResponse
}

import scala.collection.JavaConversions._

import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }

object App extends Filter with App {
  def destroy() {}
  def init(config: FilterConfig) {}

  def doFilter(req: ServletRequest, resp: ServletResponse, chain: FilterChain) {
    val hreq = req.asInstanceOf[HttpServletRequest]
    val hresp = resp.asInstanceOf[HttpServletResponse]

    hresp.setCharacterEncoding("UTF-8")

    hreq.getRequestURI match {
      case "/" if hreq.getMethod == "GET" ⇒
        home(hreq, hresp)
      case "/upload" if hreq.getMethod == "POST" ⇒
        req.getAttributeNames.foreach { n ⇒
          println(s"name: $n, value: [${req.getAttribute(n)}]")
        }

        println(req.getParameter("pdf"))
        modelFusion(hreq, hresp)
      case _ ⇒
        chain.doFilter(req, resp)
    }
  }
}

trait App extends Html {
  import java.io.InputStreamReader

  import argonaut._, Argonaut._
  import com.itextpdf.text.{ Document, Image, BaseColor }
  import com.itextpdf.text.pdf.{ PdfReader, PdfWriter, PdfStamper }

  case class Rect(x: Int, y: Int, h: Int, w: Int, name: String, typ: String)
  case class Page(areas: Option[List[Rect]])
  case class Model(format: String, pages: List[Page])

  object Rect {
    implicit def rectCodecJson =
      casecodec6(Rect.apply, Rect.unapply)("x", "y", "height", "width", "name", "type")
  }

  object Page {
    implicit def pageCodecJson =
      casecodec1(Page.apply, Page.unapply)("areas")
  }

  object Model {
    implicit def modelCodecJson =
      casecodec2(Model.apply, Model.unapply)("format", "pages")
  }

  val multipartConfigElement =
    new MultipartConfigElement("tmp", 1048576, 1048576, 262144)

  def home(req: HttpServletRequest, resp: HttpServletResponse) {
    resp.getWriter.print(index)
  }

  val pixels = 4f / 3f

  def modelFusion(req: HttpServletRequest, resp: HttpServletResponse) {
    val pdfOpt: Option[java.io.File] = Option(req.getAttribute("pdf").asInstanceOf[java.io.File])
    val jsonOpt: Option[java.io.File] = Option(req.getAttribute("json").asInstanceOf[java.io.File])
    lazy val onError = {
      resp.setStatus(400)
      resp.getWriter.println("Submitted PDF or JSON are wrong")
    }

    val res = for {
      pdfPart ← pdfOpt //if pdfPart.getContentType == "application/pdf" && pdfPart.getSize > 0
      jsonPart ← jsonOpt //  if jsonPart.getContentType == "application/json" && jsonPart.getSize >= 0
    } yield {

      def onJsonError(e: String) {
        resp.setStatus(400)
        resp.getWriter.print(e)
      }

      def onJsonSuccess(m: Model) {
        println(m)

        val reader = new PdfReader(new java.io.FileInputStream(pdfPart))
        val stamper = new PdfStamper(reader, resp.getOutputStream)

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
            over.rectangle(rect.x * pixels, pageRect.getHeight - (rect.y * pixels), rect.w * pixels, rect.h * pixels)
            over.stroke()
            over.restoreState()
          }

          i + 1
        }

        stamper.close()
        reader.close()
      }

      val jsonReader = new java.io.FileReader(jsonPart)

      Parse.decodeEither[Model](loadReader(jsonReader)).fold(onJsonError, onJsonSuccess)
    }

    res.getOrElse(onError)
  }
}
