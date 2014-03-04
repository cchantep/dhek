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
}

sealed trait BodyParserFixtures {
  lazy val parser_1_file = new FileInputStream("fixtures/parser-1")

  val parser_1_value = "value" getBytes

  val parser_1 = for {
    _ ← BodyParser.expect("key=")
    b ← BodyParser.take(5)
  } yield b
}
