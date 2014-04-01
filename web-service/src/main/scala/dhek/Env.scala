package dhek

import java.io.{ OutputStream, PrintWriter }
import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }
import scala.concurrent.ExecutionContext.Implicits.global

import argonaut._, Argonaut._
import org.eclipse.jetty.continuation.ContinuationSupport
import reactivemongo.api.MongoConnection
import reactivemongo.api.collections.default.BSONCollection
import resource.{ ManagedResource, managed }

case class Env(
    settings: Settings,
    req: HttpServletRequest,
    resp: HttpServletResponse,
    mongo: ManagedResource[MongoConnection]) {

  type Complete = () ⇒ Unit

  def async(f: Complete ⇒ Unit): Unit = {
    val continuation = ContinuationSupport.getContinuation(req)

    continuation.setTimeout(settings.timeout.toMillis)
    continuation.suspend()
    f(() ⇒ continuation.complete())
  }

  def jsonError(code: Int, msg: String) {
    resp.setStatus(code)
    resp.setContentType("application/json")
    managed(resp.getWriter).acquireAndGet {
      _.print(Json("exception" -> jString(msg)).asJson.nospaces)
    }
  }

  def writer: ManagedResource[PrintWriter] =
    managed(resp.getWriter)

  def outputStream: ManagedResource[OutputStream] =
    managed(resp.getOutputStream)

  def withUsers: ManagedResource[BSONCollection] = mongo.map { con ⇒
    val db = con(settings.dbName)

    db(settings.dbUsersName)
  }
}
