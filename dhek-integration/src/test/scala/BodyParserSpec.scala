package dhek

import java.io.FileInputStream

import org.specs2.mutable.Specification

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
    "be parsed with length" in {
      lazy val action =
        BodyParser.runParser(parser_form_data, parser_multipart_form_data_file)

      action aka "Parse result" must beRight.which { b ⇒
        b aka "Boundary" must_== parser_form_expect
      }
    }

    "be parsed with NO length" in {
      lazy val action =
        BodyParser.runParser(parser_form_data, parser_multipart_form_no_length_data_file)

      action aka "Parse result" must beRight.which {
        case b ⇒ b aka "Boundary" must_== parser_form_expect_no_length
      }
    }
  }
}

sealed trait BodyParserFixtures {
  def parser_1_file = new FileInputStream("fixtures/parser-1")
  def parser_multipart_form_data_file = new FileInputStream("fixtures/parser-multipart-form-data")
  def parser_multipart_form_no_length_data_file = new FileInputStream("fixtures/parser-multipart-form-data-no-length")

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
}
