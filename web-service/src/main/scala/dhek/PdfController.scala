package dhek

import java.io.{ File, FileInputStream, FileReader, OutputStream, PrintWriter }
import javax.servlet.http.HttpServletResponse

import argonaut._, Argonaut._
import argonaut.{ Json => ArgJson }
import com.itextpdf.text.pdf.{
  PdfDictionary,
  PdfObject,
  PdfName,
  PdfReader,
  PdfStream,
  PdfWriter,
  PdfStamper
}
import com.itextpdf.text.{
  BaseColor,
  FontFactory
}

import resource.{ ManagedResource, managed }

case class Fusion(
  pdf: ManagedResource[FileInputStream],
  model: ManagedResource[FileReader],
  font: Option[ManagedResource[FileInputStream]]
)

object PdfController {

  case class Rect(x: Int, y: Int, h: Int, w: Int, name: String, typ: String)
  case class Page(areas: Option[List[Rect]])
  case class Model(format: String, pages: List[Page])

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

  def apply(fusion: Fusion, env: Env) {

    def jsonSuccess(m: Model): Unit = {
      val action = for {
        input   <- fusion.pdf
        output  <- managed(env.resp.getOutputStream)
        reader  <- managed(new PdfReader(input))
        stamper <- managed(new PdfStamper(reader, output))
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

          env.resp.setContentType("application/pdf")

          m.pages.foldLeft(1) { (i, p) ⇒
            val page = stamper.getImportedPage(reader, i)
            val pageRect = reader.getPageSize(i)
            val over = stamper.getOverContent(i)

            for {
              rects <- p.areas.toList
              rect  <- rects
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
      }

      action.acquireAndGet(identity)
    }

    fusion.model.acquireAndGet { fm =>
      Parse.decodeEither[Model](Binaries.loadReader(fm)).
        fold(env.jsonError(400, _), jsonSuccess)
    }
  }
}
