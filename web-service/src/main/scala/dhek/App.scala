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

// TODO: Rename file to Plan.scala
final class Plan(m: ⇒ MongoConnection, s: Settings) extends Filter {

  def destroy() {}
  def init(config: FilterConfig) {}

  val auth = AuthController.make(s.secretKey)

  def doFilter(req: ServletRequest, resp: ServletResponse, chain: FilterChain) {
    val hreq = req.asInstanceOf[HttpServletRequest]
    val hresp = resp.asInstanceOf[HttpServletResponse]
    val env = Env(
      req = hreq,
      resp = hresp,
      mongo = managed(m),
      settings = s
    )

    hresp.setCharacterEncoding("UTF-8")

    hreq match {
      case POST(Path("/auth")) & ~(Param("email"), email) & ~(Param("password"), passw) ⇒ auth(Auth(email, passw), env)

      case POST(Path("/my-templates")) & ~(Param("token"), token) ⇒
        TemplateController.myTemplates(env)(token)

      case POST(Path("/rm-templates")) & ~(Param("token"), token) & ~(Params("template[]"), ids) ⇒ TemplateController.removeTemplates(env)(token, ids)

      case POST(Path("/upload")) & ~(Param("pdf"), pdf) & ~(Param("json"), json) ⇒
        val mpdf = managed(new FileInputStream(pdf.asInstanceOf[File]))
        val mjson = managed(new FileReader(json.asInstanceOf[File]))

        PdfController(Fusion(mpdf, mjson, None), env)

      case POST(Path("/save-template")) &
        ~(Param("token"), token) &
        ~(Param("pdf"), pdfName) &
        ~(Param("json"), jsonName) &
        ~(Param("name"), name) &
        ~(Attr("pdf"), pdfFile) &
        ~(Attr("json"), jsonFile) ⇒

        val pdf = FileInfo(pdfName, pdfFile.asInstanceOf[File])
        val json = FileInfo(jsonName, jsonFile.asInstanceOf[File])

        TemplateController.saveTemplate(env)(token, name, pdf, json)

      case POST(Path("/register")) & ~(Param("email"), email) & ~(Param("password"), passw) ⇒ auth.register(env, email, passw)

      case _ ⇒
        chain.doFilter(req, resp)
    }
  }
}
