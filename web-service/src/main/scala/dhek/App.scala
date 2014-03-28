package dhek

import javax.servlet.{
  Filter,
  FilterConfig,
  FilterChain,
  ServletRequest,
  ServletResponse
}
import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }

import org.eclipse.jetty.continuation.ContinuationSupport
import reactivemongo.api.MongoConnection
import resource.managed
import scalaz.~>
import scalaz.Id.Id

import Route.{
  Request,
  GetMethod,
  GetURI,
  GetAsync,
  Failed,
  GetMongo,
  GetSettings,
  GetAttrs,
  GetParams,
  GetResponse
}

final class Plan(m: => MongoConnection, s: Settings) extends Filter {

  type Eval = ({type λ[α] = Request[Option[α]]})#λ ~> Option

  def destroy() {}
  def init(config: FilterConfig) {}

  val routes: List[Routing] = List(
    AuthController.routing(s.secretKey),
    TemplateController.routing,
    PdfController.routing
  )

  val toOption: Id ~> Option = new (Id ~> Option) {
    def apply[A](fa: Id[A]): Option[A] = Some(fa)
  }

  def doFilter(req: ServletRequest, resp: ServletResponse, chain: FilterChain) {
    val hreq = req.asInstanceOf[HttpServletRequest]
    val hresp = resp.asInstanceOf[HttpServletResponse]

    def getParam(n: String): Option[String] = Option(hreq.getParameter(n))
    def getAttr(n: String): Option[Any] = Option(hreq.getAttribute(n))


    def eval: Eval = new Eval {
      def apply[A](fa: Request[Option[A]]): Option[A] = fa match {
        case GetMethod(k) => k(hreq.getMethod)
        case GetURI(k)    => k(hreq.getRequestURI)
        case GetAsync(k)  =>
          val continuation = ContinuationSupport.getContinuation(hreq)
          continuation.setTimeout(s.timeout.toMillis)
          k(Async(continuation))
        case GetSettings(k)     => k(s)
        case GetMongo(k)        => k(managed(m))
        case GetAttrs(k)        => k(getAttr _)
        case GetParams(k)       => k(getParam _)
        case GetResponse(k)     => k(hresp)
        case Failed()           => None
      }
    }

    hresp.setCharacterEncoding("UTF-8")

    val opt = routes.find { r =>
      r.route.foldRight(toOption)(eval).map(r.callback).isDefined
    }

    opt.fold(chain.doFilter(req, resp))(identity)
  }
}
