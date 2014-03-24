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
    def unapply(req: HttpServletRequest) = Option(req.getRequestURI)
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

    def unapply(req: HttpServletRequest): Option[Map[String, Any]] = Option {
      req.getAttributeNames.foldLeft(Map[String, Any]()) { (map, name) ⇒
        map + (name -> req.getAttribute(name))
      }
    }
  }

  object Params {
    import scala.collection.JavaConverters._

    def unapply(req: HttpServletRequest): Option[Map[String, Array[String]]] = Option {
      req.getParameterMap.asScala.toMap
    }
  }

  object First {
    def apply(name: String, ps: Map[String, Array[String]]): Option[String] =
      ps.get(name).flatMap(_.headOption)
  }
}
