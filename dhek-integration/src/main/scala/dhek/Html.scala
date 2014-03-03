package dhek

import java.io.{ BufferedReader, File, FileReader }

import resource.managed

trait Html {
  private def loadFile(path: String): String =
    managed(new BufferedReader(new FileReader(new File(path)))).acquireAndGet { r ⇒
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
