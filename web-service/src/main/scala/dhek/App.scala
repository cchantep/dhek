package dhek

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

final class Plan(m: => MongoConnection, s: Settings) extends Filter {

  def destroy() {}
  def init(config: FilterConfig) {}

  val routes: List[Routing] = List(
    AuthController.routing(s.secretKey),
    TemplateController.routing,
    PdfController.routing
  )

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

    val opt = routes.find { r =>
      r.route(env).map(r.callback(_, env)).isDefined
    }

    opt.fold(chain.doFilter(req, resp))(identity)
  }
}
