package dhek

import java.io.PrintWriter
import javax.servlet.http.HttpServletResponse

import resource.{ ManagedResource, managed }
import org.eclipse.jetty.continuation.Continuation

case class Async (private val continuation: Continuation) {
  type Complete = () => Unit

  def apply(f: (Complete, HttpServletResponse, ManagedResource[PrintWriter]) => Unit) {
    continuation.suspend()
    val resp = continuation.getServletResponse.
      asInstanceOf[HttpServletResponse]

    f(() => continuation.complete(), resp, managed(resp.getWriter))
  }

  def apply(f: (Complete, ManagedResource[PrintWriter]) => Unit): Unit = 
    apply((c, _, w) => f(c, w))

  def apply(f: Complete => Unit): Unit = apply((c,_) => f(c))
}
