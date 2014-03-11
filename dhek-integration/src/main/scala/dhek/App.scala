package dhek

import javax.servlet.{
  Filter,
  FilterConfig,
  FilterChain,
  ServletRequest,
  ServletResponse
}

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
        modelFusion(hreq, hresp)
      case _ ⇒
        chain.doFilter(req, resp)
    }
  }
}

trait App extends Html {
  import java.io.InputStreamReader
  import argonaut._, Argonaut._

  case class Rect(x: Int, y: Int, h: Int, w: Int, name: String, typ: String)
  case class Page(areas: Option[List[Rect]])
  case class Model(format: String, pages: List[Page])

  object Rect {
    implicit def rectCodecJson =
      casecodec6(Rect.apply, Rect.unapply)("x", "y", "height", "width", "name", "type")
  }

  object Page {
    implicit def pageCodecJson =
      casecodec1(Page.apply, Page.unapply)("pages")
  }

  object Model {
    implicit def modelCodecJson =
      casecodec2(Model.apply, Model.unapply)("format", "pages")
  }

  def home(req: HttpServletRequest, resp: HttpServletResponse) {
    resp.getWriter.print(index)
  }

  def modelFusion(req: HttpServletRequest, resp: HttpServletResponse) {
    val pdfOpt = Option(req.getPart("pdf"))
    val jsonOpt = Option(req.getPart("json"))
    lazy val onError = {
      resp.setStatus(400)
      resp.getWriter.println("Submitted PDF or JSON are wrong")
    }

    val res = for {
      pdfPart ← pdfOpt if pdfPart.getContentType == "application/pdf" && pdfPart.getSize > 0
      jsonPart ← jsonOpt if jsonPart.getContentType == "application/json" && jsonPart.getSize >= 0
    } yield {

      def onJsonError(e: String) {
        resp.setStatus(400)
        resp.getWriter.print(e)
      }

      def onJsonSuccess(m: Model) {

      }

      val jsonReader = new InputStreamReader(jsonPart.getInputStream)

      Parse.decodeEither[Model](loadReader(jsonReader)).fold(onJsonError, onJsonSuccess)
    }

    res.getOrElse(onError)
  }
}
