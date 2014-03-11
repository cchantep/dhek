package dhek

import java.io.{ BufferedReader, File, FileReader, Reader }

import resource.managed

trait Html {
  def loadFile(path: String): String =
    loadReader(new FileReader(new File(path)))

  def loadReader(r: Reader): String =
    managed(new BufferedReader(r)).acquireAndGet { r ⇒
      val buffer = new StringBuffer()

      @annotation.tailrec
      def loop: Unit = {
        val str = r.readLine

        if (str != null) {
          buffer.append(str)
          loop
        }
      }

      loop
      buffer.toString
    }

  lazy val index = loadFile("html/index.html")

}
