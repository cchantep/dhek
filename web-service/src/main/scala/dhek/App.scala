package dhek

import java.io.{ File, FileInputStream }
import javax.servlet.{
  Filter,
  FilterConfig,
  FilterChain,
  MultipartConfigElement,
  ServletRequest,
  ServletResponse
}
import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }

import reactivemongo.api.MongoConnection

import Extractor.{ &, Attr, GET, POST, Param, Path }

final class Plan(m: ⇒ MongoConnection, s: Settings)
    extends Filter with Controllers {

  def mongo: MongoConnection = m
  def settings: Settings = s

  def destroy() {}
  def init(config: FilterConfig) {}

  def doFilter(req: ServletRequest, resp: ServletResponse, chain: FilterChain) {
    val hreq = req.asInstanceOf[HttpServletRequest]
    val hresp = resp.asInstanceOf[HttpServletResponse]

    hresp.setCharacterEncoding("UTF-8")

    hreq match {
      case POST(Path("/auth")) &
          ~(Param("email"), email) & ~(Param("password"), password) ⇒
         auth(email, password, hreq, hresp)
      case POST(Path("/upload")) & ~(Attr("pdf"), pdf) & ~(Attr("json"), js) ⇒
        modelFusion(pdf.asInstanceOf[File], js.asInstanceOf[File], hresp)
      case POST(Path("/my-templates")) & ~(Param("t"), token) =>
        myTemplates(token, hreq, hresp)
     case _ ⇒ chain.doFilter(req, resp)
    }
  }
}

sealed trait Controllers { self: Plan ⇒ // TODO: Separate each controller
  import java.io.InputStreamReader
  import javax.crypto.Mac
  import javax.crypto.spec.SecretKeySpec

  import scala.util.{ Failure, Success }
  import scala.concurrent.{ Await, Future }
  import scala.concurrent.duration.Duration
  import scala.concurrent.ExecutionContext.Implicits.global

  import argonaut._, Argonaut._
  import argonaut.{ Json => ArgJson }
  import com.itextpdf.text.{
    BaseColor,
    Document,
    FontFactory,
    Image,
    Paragraph
  }
  import com.itextpdf.text.pdf.{
    PdfDictionary,
    PdfObject,
    PdfName,
    PdfReader,
    PdfStream,
    PdfWriter,
    PdfStamper
  }
  import org.apache.commons.codec.binary.Hex
  import org.apache.commons.codec.digest.DigestUtils
  import resource.managed
  import org.eclipse.jetty.continuation.{ Continuation, ContinuationSupport }
  import reactivemongo.bson.BSONDocument

  case class Rect(x: Int, y: Int, h: Int, w: Int, name: String, typ: String)
  case class Page(areas: Option[List[Rect]])
  case class Model(format: String, pages: List[Page])
  case class Template(id: String, name: String)

  // JSON codecs
  object Rect {
    implicit def rectCodecJson =
      casecodec6(Rect.apply, Rect.unapply)(
        "x", "y", "height", "width", "name", "type")
  }

  object Page {
    implicit def pageCodecJson =
      casecodec1(Page.apply, Page.unapply)("areas")
  }

  object Model {
    implicit def modelCodecJson =
      casecodec2(Model.apply, Model.unapply)("format", "pages")
  }

  object Template {
    implicit def templateCodecJson =
      casecodec2(Template.apply, Template.unapply)("id", "name")
  }

  // TODO: JSON Format in body {"exception":"message"}
  private def jsonError(resp: HttpServletResponse)(s: Int, e: String) {
    resp.setStatus(s)
    resp.getWriter.print(ArgJson("exception" -> jString(e)).nospaces)
  }
  val sha1: Mac = {
    val tmp = Mac.getInstance("HmacSHA1")
    tmp.init(new SecretKeySpec(Hex.decodeHex(settings.secretKey), "HmacSHA1"))
    tmp
  }

  def auth(email: String, passw: String, req: HttpServletRequest, resp: HttpServletResponse) {
    println(s"email = $email, password = $passw") // TODO: Logger (debug|info)

    val findTok = managed(self.mongo) map { con ⇒
      val db = con("dhek")
      db("users").find(BSONDocument("email" -> email)).one[BSONDocument]
    }

    val continuation = {
      val cont = ContinuationSupport.getContinuation(req)
      cont.setTimeout(settings.timeout.toMillis)
      cont.suspend(resp)
      cont
    }

    findTok acquireAndGet { f ⇒
      val find: Future[Option[BSONDocument]] =
        scala.concurrent.Await.ready(f, settings.timeout)

      def err(e: String, s: Int = 400) = jsonError(resp)(s, e)

      find onComplete {
        case Success(res) ⇒
          // TODO: No exception as JSON on mismatch cases
          val action = for {
            doc ← res
            pwd ← doc.getAs[String]("password")
          } yield {
            val resp = continuation.getServletResponse.
              asInstanceOf[HttpServletResponse]

            if (pwd == DigestUtils.sha1Hex(passw)) {
              val token = Hex.encodeHexString(sha1.doFinal(email.getBytes))

              resp.setContentType("application/json")
              managed(resp.getWriter).acquireAndGet(_.
                print(ArgJson("token" -> jString(token)).nospaces))

            } else err("Authentication mismatch") // password mismatch
          }

          action.getOrElse(err("Authentication mismatch")/* user mismatch */)
          continuation.complete()
        case Failure(e) ⇒
          e.printStackTrace() // TODO: logging
          err(s"Fails to check authentication: ${e.getMessage}")
          continuation.complete()
      }
    }
  }

  def modelFusion(pdf: File, json: File, resp: HttpServletResponse, fontUrl: Option[File] = None) {

    println(s"pdf = $pdf, json = $json") // TODO: Logging (debug|info)

    def onJsonSuccess(m: Model): Unit = (for {
      reader <- managed(new PdfReader(new java.io.FileInputStream(pdf)))
      stamper <- managed(new PdfStamper(reader, resp.getOutputStream))
    } yield {
      def foreachObject[U](f: PdfObject ⇒ U) {
        val objCount = reader.getXrefSize

        @annotation.tailrec
        def loop(cur: Int) {
          if (cur < objCount) {
            f(reader.getPdfObject(cur - 1))
            loop(cur + 1)
          }
        }

        loop(1)
      }

      resp.setContentType("application/pdf")

      m.pages.foldLeft(1) { (i, p) ⇒
        val page = stamper.getImportedPage(reader, i)
        val pageRect = reader.getPageSize(i)
        val over = stamper.getOverContent(i)

        for {
          rects ← p.areas.toList
          rect ← rects
        } yield {
          over.saveState()
          over.setLineWidth(2)
            over.setColorStroke(BaseColor.RED)
          over.rectangle(rect.x, pageRect.getHeight - rect.y - rect.h, rect.w, rect.h)
          over.stroke()
          over.restoreState()
        }

        i + 1
      }
    }) acquireFor { _ => () }

    lazy val jsonReader = new java.io.FileReader(json)

    Parse.decodeEither[Model](Binaries.loadReader(jsonReader)).
      fold(jsonError(resp)(400, _), onJsonSuccess(_))
  }

  def myTemplates(token: String, req: HttpServletRequest, resp: HttpServletResponse) {
    def bsonToTemplate(b: BSONDocument): Template = {
      val action = for {
        id <- b.getAs[String]("id")
        name <- b.getAs[String]("name")
      } yield Template(id, name)

      action.get // TODO: Handle error case
    }

    val findTemplates = managed(self.mongo) map { con ⇒
      val db = con("dhek")
      db("users").find(BSONDocument("adminToken" -> token)).one[BSONDocument]
    }

    val continuation = {
      val cont = ContinuationSupport.getContinuation(req)
      cont.suspend(resp)
      cont.setTimeout(settings.timeout.toMillis)
      cont
    }

    findTemplates.acquireAndGet { f =>
      val find: Future[Option[BSONDocument]] =
        scala.concurrent.Await.ready(f, settings.timeout)

      val hresp = continuation.getServletResponse.
            asInstanceOf[HttpServletResponse]

      find onComplete {
        case Success(res) =>
          val action = for {
            doc <- res
            tempsBSON <- doc.getAs[List[BSONDocument]]("templates").orElse(Some(Nil))
            templates <- Some(tempsBSON.map(bsonToTemplate))
          } yield templates

          hresp.setContentType("application/json")
          action match {
            case None     => hresp.setStatus(404)
            case Some(ts) =>
              managed(hresp.getWriter).acquireAndGet(_.
                print(ts.asJson.nospaces))
          }
          continuation.complete()
        case Failure(e) =>
          e.printStackTrace() // TODO: Log
          jsonError(hresp)(500, "")
          continuation.complete()
      }
    }
  }
}
