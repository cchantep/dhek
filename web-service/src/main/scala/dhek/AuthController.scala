package dhek

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.Future
import scala.util.{ Failure, Success }

import argonaut._, Argonaut._
import argonaut.{ Json ⇒ ArgJson }
import org.apache.commons.codec.binary.Hex
import org.apache.commons.codec.digest.DigestUtils.sha1Hex
import reactivemongo.bson.BSONDocument
import resource.managed

case class Auth(email: String, passw: String)

case class AuthController private (secretKey: Array[Char]) {
  /* TODO: Will be moved to user registration
  val sha1: Mac = {
    val tmp = Mac.getInstance("HmacSHA1")

    tmp.init(new SecretKeySpec(Hex.decodeHex(secretKey), "HmacSHA1"))
    tmp
  }
   */

  def apply(auth: Auth, env: Env) {
    val findToken = env.mongo.map { con ⇒
      val db = con("dhek")

      db("users").find(BSONDocument("email" -> auth.email)).one[BSONDocument]
    }

    env.async { complete ⇒
      findToken.acquireAndGet { ft ⇒
        val future: Future[Option[BSONDocument]] =
          Await.ready(ft, env.settings.timeout)

        env.resp.setContentType("application/json")

        future.onComplete {
          case Success(res) ⇒
            val token: Option[String] = for {
              doc ← res
              pwd ← doc.getAs[String]("password") if pwd == sha1Hex(auth.passw)
              tok ← doc.getAs[String]("adminToken")
            } yield tok

            env.writer.acquireAndGet(_.print(token.fold[String]("null")(tok ⇒
              ArgJson("token" -> jString(tok)).nospaces)))

            complete()
          case Failure(e) ⇒
            e.printStackTrace()
            env.jsonError(500, e.getMessage)
            complete()
        }
      }
    }
  }
}

object AuthController {
  def make(secretKey: Array[Char]): AuthController = AuthController(secretKey)
}
