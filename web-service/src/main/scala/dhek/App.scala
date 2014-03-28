package dhek

import java.io.{ File, FileInputStream, FileReader }
import javax.servlet.{
  Filter,
  FilterConfig,
  FilterChain,
  ServletRequest,
  ServletResponse
}
import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }
import reactivemongo.api.MongoConnection
import resource.managed

import Extractor.{ POST, Path, Param, Params, Attr, & }

final class Plan(m: => MongoConnection, s: Settings) extends Filter {

  def destroy() {}
  def init(config: FilterConfig) {}

  val auth = AuthController.make(s.secretKey)

  def doFilter(req: ServletRequest, resp: ServletResponse, chain: FilterChain) {
    val hreq = req.asInstanceOf[HttpServletRequest]
    val hresp = resp.asInstanceOf[HttpServletResponse]
    val env = Env (
      req      = hreq,
      resp     = hresp,
      mongo    = managed(m),
      settings = s
    )

    hresp.setCharacterEncoding("UTF-8")

    hreq match {
      case POST(Path("/auth")) & ~(Param("email"), email) & ~(Param("password"), passw) =>
        auth(Auth(email, passw), env)
      case POST(Path("/my-templates")) & ~(Param("token"), token) =>
        TemplateController(GetTemplates(token), env)
      case POST(Path("/upload")) & ~(Param("pdf"), pdf) & ~(Param("json"), json) =>
        val mpdf  = managed(new FileInputStream(pdf.asInstanceOf[File]))
        val mjson = managed(new FileReader(json.asInstanceOf[File]))

        PdfController(Fusion(mpdf, mjson, None), env)
      case POST(Path("/rm-templates")) & ~(Param("token"), token) & ~(Params("template[]"), tps) =>
        TemplateController(DeleteTemplates(token, tps), env)
      case _ => chain.doFilter(req, resp)
    }
  }
}
