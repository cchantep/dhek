package dhek

import dhek.BodyParser.Parser

object Form {

  case class ContentType(value: String)

  case class Boundary(name: String, length: Option[Int])

  val `;` = ';' toByte
  val `\n` = '\n' toByte
  val space = ' ' toByte

  def notWhitespace(b: Byte): Boolean =
    b != `\n` && b != space

  def header: Parser[ContentType] =
    for {
      _ ← BodyParser.expect("Content-Type: ")
      bs ← BodyParser.takeWhile(_ != `;`)
      _ ← BodyParser.getByte
    } yield ContentType(new String(bs))

  def boundary: Parser[Boundary] = {
    val contentLength = for {
      _ ← BodyParser.getByte
      _ ← BodyParser.expect("Content-Length: ")
      l ← BodyParser.integer()
    } yield l

    for {
      _ ← BodyParser.expect("boundary=")
      bs ← BodyParser.takeWhile1(notWhitespace)
      l ← BodyParser.optional(contentLength)
    } yield Boundary(new String(bs), l)
  }
}
