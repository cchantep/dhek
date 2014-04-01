package dhek

import java.io.{
  BufferedReader,
  ByteArrayOutputStream,
  File,
  FileReader,
  Reader,
  OutputStream,
  FileInputStream,
  FileOutputStream,
  InputStream
}

import resource.managed

object Binaries {
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
  lazy val login = loadFile("webui/login.html")

  def writeTo(input: ⇒ InputStream, output: ⇒ OutputStream) {
    managed(input).acquireAndGet { in ⇒
      managed(output).acquireAndGet { out ⇒
        val buffer = new Array[Byte](8192)

        @annotation.tailrec
        def loop: Unit = {
          val read = in.read(buffer)

          if (read > -1) {
            out.write(buffer.slice(0, read))
            loop
          }
        }

        loop
      }
    }
  }

  def loadRawBytes(input: ⇒ InputStream): Array[Byte] = {
    val output = new ByteArrayOutputStream

    writeTo(input, output)
    output.toByteArray
  }

  def writeFileTo(path: String, output: ⇒ OutputStream) =
    writeTo(new FileInputStream(path), output)
}
