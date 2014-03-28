package dhek

import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }

import org.eclipse.jetty.continuation.ContinuationSupport
import reactivemongo.api.MongoConnection
import resource.ManagedResource

case class Env(
  settings: Settings,
  req: HttpServletRequest,
  resp:HttpServletResponse,
  mongo: ManagedResource[MongoConnection]) {

  type Complete = () => Unit

  def async(f: Complete => Unit): Unit = {
    val continuation = ContinuationSupport.getContinuation(req)

    continuation.setTimeout(settings.timeout.toMillis)
    continuation.suspend()
    f(() => continuation.complete())
  }
}
