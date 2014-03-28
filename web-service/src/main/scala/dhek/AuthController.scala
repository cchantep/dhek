package dhek

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.Future
import scala.util.{ Failure, Success }

import argonaut._, Argonaut._
import argonaut.{ Json => ArgJson }
import org.apache.commons.codec.binary.Hex
import org.apache.commons.codec.digest.DigestUtils
import reactivemongo.bson.BSONDocument
import resource.managed

case class Auth( email: String, passw: String)

case class AuthController private(secretKey: Array[Char]) {

  val sha1: Mac = {
    val tmp = Mac.getInstance("HmacSHA1")

    tmp.init(new SecretKeySpec(Hex.decodeHex(secretKey), "HmacSHA1"))
    tmp
  }

  def apply(auth: Auth, env: Env) {
    val findToken = env.mongo.map { con =>
      val db = con("dhek")

      db("users").find(BSONDocument("email" -> auth.email)).one[BSONDocument]
    }

    env.async { complete =>
      findToken.acquireAndGet { ft =>
        val future: Future[Option[BSONDocument]] =
          Await.ready(ft, env.settings.timeout)

        env.resp.setContentType("application/json")
        val writer = managed(env.resp.getWriter)

        future.onComplete {
          case Success(res) =>
            val action = for {
              doc <- res
              pwd <- doc.getAs[String]("password")
           } yield (pwd == DigestUtils.sha1Hex(auth.passw))

           action match {
             case Some(matched) if matched =>
               val token =
                 Hex.encodeHexString(sha1.doFinal(auth.email.getBytes))

               writer.acquireAndGet(
                 _.print(ArgJson("token" -> jString(token)).nospaces)
               )

             case _ =>
               val msg = "Authentication mismatch"

               writer.acquireAndGet(
                 _.print(ArgJson("exception" -> jString(msg)).nospaces)
               )
            }

            complete()
          case Failure(e) =>
            e.printStackTrace()
            env.resp.setStatus(400)
            writer.acquireAndGet(
              _.print(ArgJson("exeption" -> jString(e.getMessage)).nospaces)
            )
            complete()
        }
      }
    }
  }
}

object AuthController {
  def make(secretKey: Array[Char]): AuthController = AuthController(secretKey)
}
