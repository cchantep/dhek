package dhek

import java.io.PrintWriter
import javax.servlet.ServletOutputStream
import javax.servlet.http.HttpServletResponse

import reactivemongo.api.MongoConnection
import resource.ManagedResource
import scalaz.{ Functor, Free }
import scalaz.Free.{ Return, Suspend }

sealed trait Routing {
  type A

  val route: Route.R[A]
  val callback: A => Unit
}

object Routing {
  def apply[B](r: Route.R[B], c: B => Unit) = new Routing {
    type A = B

    val route = r
    val callback = c
  }
}

object Route {

  type Attrs = String => Option[Any]
  type Params = String => Option[String]
  type R[A] = Free[Request, A]

  sealed trait Request[A]
  case class GetMethod[A](k: String => A) extends Request[A]
  case class GetURI[A](k: String => A) extends Request[A]
  case class GetSettings[A](k: Settings => A) extends Request[A]
  case class GetAsync[A](k: Async => A) extends Request[A]
  case class GetAttrs[A](k: Attrs => A) extends Request[A]
  case class GetParams[A](k: Params => A) extends Request[A]
  case class GetMongo[A](k: ManagedResource[MongoConnection] => A) extends Request[A]
  case class GetResponse[A](k: HttpServletResponse => A) extends Request[A]
  case class Failed[A]() extends Request[A]

  object Request {
    implicit val requestFunctor = new Functor[Request] {
      def map[A, B](fa: Request[A])(f: A => B): Request[B] = fa match {
        case GetMethod(k)       => GetMethod(f compose k)
        case GetURI(k)          => GetURI(f compose k)
        case GetSettings(k)     => GetSettings(f compose k)
        case GetAsync(k)        => GetAsync(f compose k)
        case GetAttrs(k)        => GetAttrs(f compose k)
        case GetParams(k)       => GetParams(f compose k)
        case GetMongo(k)        => GetMongo(f compose k)
        case GetResponse(k)     => GetResponse(f compose k)
        case Failed()           => Failed()
      }
    }
  }

  def value[A](v: A): R[A] = Return(v)

  def getMethod: R[String] = Suspend(GetMethod(Return(_)))

  def getURI: R[String] = Suspend(GetURI(Return(_)))

  def getSettings: R[Settings] = Suspend(GetSettings(Return(_)))

  def getAsync: R[Async] = Suspend(GetAsync(Return(_)))

  def getAttrs: R[Attrs] = Suspend(GetAttrs(Return(_)))

  def getParams: R[Params] = Suspend(GetParams(Return(_)))

  def getMongo: R[ManagedResource[MongoConnection]] =
    Suspend(GetMongo(Return(_)))

  def getResponse: R[HttpServletResponse] = Suspend(GetResponse(Return(_)))

  def failed[A]: R[A] = Suspend(Failed())

  def isGET: R[Unit] = getMethod.flatMap {
    case "GET" => Return(())
    case _     => failed[Unit]
  }

  def isPOST: R[Unit] = getMethod.flatMap {
    case "POST" => Return(())
    case _      => failed[Unit]
  }

  def hasURI(s: String): R[Unit] = getURI.flatMap {
    case uri if uri == s => Return(())
    case _               => failed[Unit]
  }
}
