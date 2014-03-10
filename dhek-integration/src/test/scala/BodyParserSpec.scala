package dhek

import java.io.{ ByteArrayOutputStream, FileInputStream }

import org.specs2.mutable.Specification

import BodyParser.Parser

object BodyParserSpec extends Specification with BodyParserFixtures {
  "Body Parser" title

  "File" should {
    "be parsed" in {
      BodyParser.runParser(parser_1, parser_1_file) aka "Parse result" must beRight.which { b ⇒
        b aka "array of bytes" must_== (parser_1_value)
      }
    }
  }

  "multipart/form-data" should {
    "be parsed with length and extracted Boundary value" in {
      lazy val action =
        BodyParser.runParser(parser_form_data_complete, parser_multipart_form_data_file)

      action aka "Parse result" must beRight.which { b ⇒
        b aka "Boundary value" must_== "Larry"
      }
    }

    "be parsed with no Length" in {
      lazy val action =
        BodyParser.runParser(parser_form_data, parser_multipart_form_no_length_data_file)

      action aka "Parse result" must beRight.which {
        case b ⇒ b aka "Boundary" must_== parser_form_expect_no_length
      }
    }

    "be parsed with PDF" in {
      lazy val action =
        BodyParser.runParser(parser_file, parser_pdf_file)

      action aka "Parse result" must beRight.which {
        case b ⇒ b aka "Content-Type" must_== "application/pdf" // parser_form_expect_no_length
      }
    }
  }
}

sealed trait BodyParserFixtures {
  def parser_1_file = new FileInputStream("fixtures/parser-1")
  def parser_multipart_form_data_file = new FileInputStream("fixtures/parser-multipart-form-data")
  def parser_multipart_form_no_length_data_file = new FileInputStream("fixtures/parser-multipart-form-data-no-length")
  def parser_pdf_file = new FileInputStream("fixtures/pdf-file")

  val parser_1_value = "value" getBytes

  val parser_1 = for {
    _ ← BodyParser.expect("key=")
    b ← BodyParser.take(5)
  } yield b

  val parser_form_expect = Form.Boundary("--AaB03x", Some(300))
  val parser_form_expect_no_length = Form.Boundary("--AaB03x", None)

  val parser_form_data = for {
    _ ← Form.header
    _ ← BodyParser.expect(" ")
    b ← Form.boundary
  } yield b

  val parser_form_data_complete = for {
    _ ← Form.header
    _ ← BodyParser.is(' ')
    b ← Form.boundary
    _ ← BodyParser.expect("\n\n\n")
    _ ← BodyParser.expect(s"${b.name}\n")
    _ ← BodyParser.expect("Content-Disposition: ")
    d ← BodyParser.takeWhile1(_ != ';'.toByte)
    _ ← BodyParser.expect("; ")
    _ ← BodyParser.expect("name=")
    n ← BodyParser.doubleQuotedValue()
    _ ← BodyParser.expect("\n\n\n")
    v ← BodyParser.word()
    _ ← BodyParser.getByte
    _ ← BodyParser.expect(s"${b.name}")
  } yield v

  val parser_file = {

    def action(buffer: ByteArrayOutputStream, length: Int): Parser[Unit] = {

      def decideSize(cur: Int): Option[Int] =
        if (cur + 8192 <= length) None
        else Some(length - cur)

      def loop(cur: Int): Parser[Unit] = {
        if (cur == length)
          BodyParser.unit
        else for {
          bs ← BodyParser.buffer(decideSize(cur))
          _ ← if (bs.length == 0) BodyParser.failure(s"Unexpected EOF $cur $length") else BodyParser.unit
          _ ← BodyParser.beware(println(s"cur: $cur, length: $length, read: ${bs.length}"))
          _ ← BodyParser.beware(buffer.write(bs))
          _ ← loop(cur + bs.length)
        } yield ()
      }

      loop(0)
    }

    for {
      _ ← Form.header
      _ ← BodyParser.is(' ')
      b ← Form.boundary
      _ ← BodyParser.expect("\n\n")
      _ ← BodyParser.expect(s"--${b.name}\n")
      _ ← BodyParser.expect("Content-Disposition: ")
      d ← BodyParser.takeWhile1(_ != ';'.toByte)
      _ ← BodyParser.expect("; ")
      _ ← BodyParser.expect("name=")
      name ← BodyParser.doubleQuotedValue()
      _ ← BodyParser.expect("; filename=")
      filename ← BodyParser.doubleQuotedValue()
      _ ← BodyParser.expect("\nContent-Type: ")
      ctype ← BodyParser.word()
      _ ← BodyParser.expect("\n\n")
      tuple ← BodyParser.alloc(new ByteArrayOutputStream())(_.close)
      _ ← action(tuple._1, b.length.get) // will do than in final code
      // _ ← BodyParser.expect(s"--${b.name}--\n") // normal end of parsing
      //_ ← BodyParser.take(b.length.get) // sounds like content-length is a lie ?
      w ← BodyParser.word()
      _ ← BodyParser.beware(println(w))
    } yield filename
  }
}
