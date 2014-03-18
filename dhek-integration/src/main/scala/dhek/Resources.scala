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

import resource.managed

import Extractor.{ &, GET, Path, Seg }

object Resources extends Filter with Html {
  def destroy() {}
  def init(config: FilterConfig) {}

  def doFilter(req: ServletRequest, resp: ServletResponse, chain: FilterChain) {
    val hreq = req.asInstanceOf[HttpServletRequest]
    val hresp = resp.asInstanceOf[HttpServletResponse]

    hreq match {
      case GET(Path(Seg(_ :: _) & p)) if fileExists(s"webui/${p.tail}") ⇒
        val ext = extension(p)

        hresp.setContentType(mime(ext))
        writeFileTo(s"webui/${p.tail}", hresp.getOutputStream)
      case _ ⇒
        chain.doFilter(req, resp)
    }
  }

  def fileExists(p: String): Boolean =
    new File(p).exists

  def extension(p: String): String = {
    val idx = p.lastIndexOf('.')

    p.drop(idx + 1)
  }

  def mime(e: String): String = e match {
    case "css"  ⇒ "text/css"
    case "json" ⇒ "application/json"
    case "js"   ⇒ "application/javascript"
  }
}
