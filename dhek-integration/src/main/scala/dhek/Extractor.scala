package dhek

import javax.servlet.http.HttpServletRequest

object Extractor {
  object & {
    def unapply[A](a: A) = Some(a, a)
  }

  object GET {
    def unapply(req: HttpServletRequest) = req.getMethod match {
      case "GET" ⇒ Some(req)
      case _     ⇒ None
    }
  }

  object POST {
    def unapply(req: HttpServletRequest) = req.getMethod match {
      case "POST" ⇒ Some(req)
      case _      ⇒ None
    }
  }

  object Path {
    def unapply(req: HttpServletRequest) =
      Some(req.getRequestURI)
  }

  object Seg {
    def unapply(path: String): Option[List[String]] =
      path.split('/').toList match {
        case Nil     ⇒ Some(Nil)
        case _ :: xs ⇒ Some(xs)
      }
  }

  object Attributes {
    import scala.collection.JavaConversions._

    def unapply(req: HttpServletRequest): Option[Map[String, Any]] = {
      val attrs =
        req.getAttributeNames.foldLeft(Map[String, Any]()) { (map, name) ⇒
          map + (name -> req.getAttribute(name))
        }

      Some(attrs)
    }
  }
}
