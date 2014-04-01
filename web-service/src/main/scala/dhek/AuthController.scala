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
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument
import reactivemongo.core.commands.LastError
import resource.managed

case class Auth(email: String, passw: String)

case class AuthController private (appSecretKey: Array[Char], secretKey: Array[Char]) {

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

  val adminMac: Mac = {
    val tmp = Mac.getInstance("HmacSHA1")

    tmp.init(new SecretKeySpec(Hex.decodeHex(secretKey), "HmacSHA1"))
    tmp
  }

  val appMac: Mac = {
    val tmp = Mac.getInstance("HmacSHA1")

    tmp.init(new SecretKeySpec(Hex.decodeHex(appSecretKey), "HmacSHA1"))
    tmp
  }

  object EmailAlreadyExists extends Exception

  def register(env: Env, email: String, passw: String) {
    env async { complete ⇒
      env.withUsers.acquireAndGet { users ⇒
        def userByEmail: Future[Option[BSONDocument]] =
          users.find(BSONDocument("email" -> email)).one[BSONDocument]

        def createUser: Future[String] = {
          val emailBytes = email.getBytes("UTF-8")
          val adminToken =
            Hex.encodeHexString(adminMac.doFinal(emailBytes))
          val appToken =
            Hex.encodeHexString(appMac.doFinal(emailBytes))
          val user = BSONDocument(
            "email" -> email,
            "password" -> sha1Hex(passw),
            "adminToken" -> adminToken,
            "appToken" -> appToken
          )

          users.insert(user) flatMap {
            case e if e.inError ⇒ Future.failed(e.fillInStackTrace)
            case _              ⇒ Future(adminToken)
          }
        }

        val future = for {
          uOpt ← userByEmail
          token ← uOpt.fold(createUser)(_ ⇒ Future.failed(EmailAlreadyExists))
        } yield token

        env.resp.setContentType("application/json")

        Await.ready(future, env.settings.timeout) onComplete {
          case Success(token) ⇒
            env.writer.acquireAndGet(
              _.print(ArgJson("token" -> jString(token)).nospaces))
            complete()
          case Failure(e) ⇒
            e match {
              case EmailAlreadyExists ⇒
                env.resp.setStatus(400)
                env.writer.acquireAndGet(
                  _.print(ArgJson("exception" -> jString("Email already exists"))))
              case e ⇒
                e.printStackTrace()
                env.jsonError(500, e.getMessage)
            }
            complete()
        }
      }
    }
  }
}

object AuthController {
  def make(appSecretKey: Array[Char], secretKey: Array[Char]): AuthController =
    AuthController(appSecretKey, secretKey)
}
