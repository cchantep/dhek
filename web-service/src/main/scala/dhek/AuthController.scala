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
import reactivemongo.api.MongoConnection
import reactivemongo.bson.BSONDocument
import resource.ManagedResource

import Extractor.{ &, Param }

case class AuthController(secretKey: Array[Char]) {

  val sha1: Mac = {
    val tmp = Mac.getInstance("HmacSHA1")

    tmp.init(new SecretKeySpec(Hex.decodeHex(secretKey), "HmacSHA1"))
    tmp
  }

  case class Auth(
    email: String,
    passw: String,
    async: Async,
    mongo: ManagedResource[MongoConnection],
    settings: Settings
  )

  def extractAuth(ps: Route.Params) = ps match {
    case ~(Param("email"), email) & ~(Param("password"), pwd) =>
      for {
        async    <- Route.getAsync
        mongo    <- Route.getMongo
        settings <- Route.getSettings
      } yield Auth(email, pwd, async, mongo, settings)
    case _ => Route.failed[Auth]
  }

  def route = for {
    _  <- Route.isPOST
    _  <- Route.hasURI("/auth")
    ps <- Route.getParams
    a  <- extractAuth(ps)
  } yield a

  def apply(auth: Auth) {
    val findToken = auth.mongo.map { con =>
      val db = con("dhek")

      db("users").find(BSONDocument("email" -> auth.email)).one[BSONDocument]
    }

    auth.async { (complete, resp, writer) =>
      findToken.acquireAndGet { ft =>
        val future: Future[Option[BSONDocument]] =
          Await.ready(ft, auth.settings.timeout)

        resp.setContentType("application/json")

        future.onComplete {
          case Success(res) =>
            val action = for {
              doc ← res
              pwd ← doc.getAs[String]("password")
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
  def routing(secretKey: Array[Char]): Routing = {
    val auth = AuthController(secretKey)

    Routing(auth.route, auth.apply)
  }
}
