package dhek

import dhek.BodyParser.{
  Parser,
  expect,
  getByte,
  optional,
  takeWhile
}

object Form {

  case class ContentType(value: String)

  case class Boundary(name: String, length: Option[Int])

  val `;` = ';' toByte
  val `\n` = '\n' toByte
  val space = ' ' toByte

  def notWhitespace(b: Byte): Boolean =
    b != `\n` && b != space

  def digit(b: Byte): Boolean =
    b >= '0'.toByte && b <= '9'.toByte

  def header: Parser[ContentType] =
    for {
      _ ← expect("Content-Type: ")
      bs ← takeWhile(_ != `;`)
      _ ← getByte
    } yield ContentType(new String(bs))

  def boundary: Parser[Boundary] = {
    val contentLength = for {
      _ ← getByte
      _ ← expect("Content-Length: ")
      l ← takeWhile(digit).map(b ⇒ new String(b).toInt)
    } yield l

    for {
      _ ← expect("boundary=")
      bs ← takeWhile(notWhitespace)
      l ← optional(contentLength)
    } yield Boundary(new String(bs), l)
  }
}
