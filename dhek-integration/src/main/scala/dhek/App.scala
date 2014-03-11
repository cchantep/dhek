package dhek

import javax.servlet.{
  Filter,
  FilterConfig,
  FilterChain,
  ServletRequest,
  ServletResponse
}

import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }

object App extends Filter with Html {
  def destroy() {}
  def init(config: FilterConfig) {}

  def doFilter(req: ServletRequest, resp: ServletResponse, chain: FilterChain) {
    val hreq = req.asInstanceOf[HttpServletRequest]
    val hresp = resp.asInstanceOf[HttpServletResponse]

    hresp.setCharacterEncoding("UTF-8")

    hreq.getRequestURI match {
      case "/" if hreq.getMethod == "GET" ⇒
        hresp.getWriter.print(index)
      case "/upload" if hreq.getMethod == "POST" ⇒
        hresp.getWriter.print("OK")
      case _ ⇒
        chain.doFilter(req, resp)
    }
  }
}
