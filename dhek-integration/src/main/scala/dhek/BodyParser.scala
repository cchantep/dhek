package dhek

import java.io.{
  ByteArrayOutputStream,
  InputStream,
  IOException,
  PushbackInputStream
}

import resource.managed

object BodyParser {
  trait Parser[+A] {

    def map[B](f: A ⇒ B): Parser[B] =
      flatMap(a ⇒ Return(f(a)))

    def flatMap[B](f: A ⇒ Parser[B]): Parser[B] = this match {
      case Bind(m, k) ⇒ Bind(m, (x: Any) ⇒ Bind(() ⇒ k(x), f))
      case m          ⇒ Bind(() ⇒ m, f)
    }

    def >>[B](n: ⇒ Parser[B]): Parser[B] =
      flatMap(_ ⇒ n)
  }

  type ReleaseKey = Int

  case class Get[A](k: Byte ⇒ Parser[A]) extends Parser[A]
  case class Peek[A](k: Byte ⇒ Parser[A]) extends Parser[A]
  case class Take[A](n: Int, k: Array[Byte] ⇒ Parser[A]) extends Parser[A]
  case class Buffer[A](max: Option[Int], k: Array[Byte] ⇒ Parser[A]) extends Parser[A]
  case class Return[A](v: A) extends Parser[A]
  case class Alloc[A, B](ack: () ⇒ A, release: A ⇒ Unit, k: (A, ReleaseKey) ⇒ Parser[B]) extends Parser[B]
  case class Release[A](key: ReleaseKey, next: Parser[A]) extends Parser[A]
  case class Failure(e: Either[String, Throwable]) extends Parser[Nothing]
  case class Bind[A, B](m: () ⇒ Parser[A], f: A ⇒ Parser[B]) extends Parser[B]

  def unit: Parser[Unit] =
    Return(())

  def getByte: Parser[Byte] =
    Get(Return(_))

  def peekByte: Parser[Byte] =
    Peek(Return(_))

  def take(n: Int): Parser[Array[Byte]] =
    Take(n, Return(_))

  def buffer(max: Option[Int] = Some(8192)): Parser[Array[Byte]] =
    Buffer(max, Return(_))

  def alloc[R](ack: ⇒ R, release: R ⇒ Unit): Parser[(R, ReleaseKey)] =
    Alloc[R, (R, ReleaseKey)](() ⇒ ack, release, (r, key) ⇒ Return((r, key)))

  def release(key: ReleaseKey): Parser[Unit] =
    Release(key, Return(()))

  def takeWhile(p: Byte ⇒ Boolean): Parser[Array[Byte]] = {
    val buffer = new ByteArrayOutputStream()

    def shift: Parser[Unit] =
      getByte.map(b ⇒ buffer.write(b))

    def loop: Parser[Array[Byte]] =
      for {
        b ← peekByte
        bs ← if (p(b)) shift >> loop else Return(buffer.toByteArray)
      } yield bs

    loop
  }

  def expect(s: String): Parser[Unit] = {
    def loop(cur: String): Parser[Unit] =
      if (cur.isEmpty) Return(())
      else {
        val c = cur.head

        getByte.flatMap { x ⇒
          if (c.toByte == x) loop(cur.tail)
          else failure(s"Expectation $s: Got ${x.toChar}, Expected: $c")
        }
      }

    loop(s)
  }

  def failure(msg: String): Parser[Nothing] =
    Failure(Left(msg))

  def throwable(e: Throwable): Parser[Nothing] =
    Failure(Right(e))

  def beware[A](v: ⇒ A): Parser[A] =
    try Return(v) catch {
      case e: Throwable ⇒ throwable(e)
    }

  private lazy val unexpectedEOF: Either[Throwable, Nothing] =
    Left(new RuntimeException("End of Stream"))

  private lazy val emptyBuffer: Array[Byte] = Array()

  def runParser[A](p: Parser[A], input: ⇒ InputStream, bsize: Int = 8192): Either[List[Throwable], A] =
    managed(new PushbackInputStream(input)).acquireFor { s ⇒
      val buffer = new Array[Byte](bsize)
      val map = scala.collection.mutable.Map[Int, () ⇒ Unit]()
      var keyCnt = 0

      def readByte(k: Byte ⇒ Parser[Any]): Either[Throwable, Parser[Any]] = {
        val b = s.read

        if (b == -1)
          unexpectedEOF
        else
          Right(k(b.toByte))
      }

      def peekByte(k: Byte ⇒ Parser[Any]): Either[Throwable, Parser[Any]] = {
        val b = s.read

        if (b == -1)
          unexpectedEOF
        else {
          s.unread(b)

          Right(k(b.toByte))
        }
      }

      def takeBytes(n: Int, k: Array[Byte] ⇒ Parser[Any]): Either[Throwable, Parser[Any]] =
        if (n <= bsize) {
          val readByte = s.read(buffer, 0, n)

          if (readByte < n)
            unexpectedEOF
          else
            Right(k(buffer.slice(0, n)))
        } else {
          val tmpBuffer = new Array[Byte](n)
          val readByte = s.read(tmpBuffer)

          if (readByte < n)
            unexpectedEOF
          else
            Right(k(tmpBuffer))
        }

      def readBytes(max: Option[Int], k: Array[Byte] ⇒ Parser[Any]): Parser[Any] = {
        val tmpBuffer = max.fold(buffer) {
          case limit if limit <= bsize ⇒ buffer
          case limit                   ⇒ new Array[Byte](limit)
        }

        val readBytes = s.read(tmpBuffer, 0, bsize)

        if (readBytes == -1)
          k(emptyBuffer)
        else
          k(tmpBuffer.slice(0, readBytes))
      }

      def doAlloc(ack: () ⇒ Any, release: Any ⇒ Unit, k: (Any, ReleaseKey) ⇒ Parser[Any]): Parser[Any] = {
        val r = ack()
        val curKey = keyCnt
        map += (keyCnt, () ⇒ release(r))
        keyCnt += 1
        k(r, curKey)
      }

      def doRelease(key: ReleaseKey, next: Parser[Any]): Parser[Any] = {
        map.get(key).foreach { finalizer ⇒
          finalizer()
          map - key
        }

        next
      }

      def reportError(e: Either[String, Throwable]): Throwable =
        e.fold(m ⇒ new IOException(m), identity)

      @annotation.tailrec
      def loop(step: Parser[Any]): A = step match {
        case Get(k) ⇒ readByte(k) match {
          case Left(e)     ⇒ throw e
          case Right(next) ⇒ loop(next)
        }
        case Peek(k) ⇒ peekByte(k) match {
          case Left(e)     ⇒ throw e
          case Right(next) ⇒ loop(next)
        }
        case Take(n, k) ⇒ takeBytes(n, k) match {
          case Left(e)     ⇒ throw e
          case Right(next) ⇒ loop(next)
        }
        case Buffer(m, k)           ⇒ loop(readBytes(m, k))
        case Alloc(ack, release, k) ⇒ loop(doAlloc(ack, release, k))
        case Release(key, next)     ⇒ loop(doRelease(key, next))
        case Return(a)              ⇒ a.asInstanceOf[A]
        case Failure(e)             ⇒ throw reportError(e)
        case Bind(m, f) ⇒ m() match {
          case Get(k) ⇒ readByte(k) match {
            case Left(e)     ⇒ throw e
            case Right(next) ⇒ loop(Bind(() ⇒ next, f))
          }
          case Peek(k) ⇒ peekByte(k) match {
            case Left(e)     ⇒ throw e
            case Right(next) ⇒ loop(Bind(() ⇒ next, f))
          }
          case Take(n, k) ⇒ takeBytes(n, k) match {
            case Left(e)     ⇒ throw e
            case Right(next) ⇒ loop(Bind(() ⇒ next, f))
          }
          case Buffer(m, k)           ⇒ loop(Bind(() ⇒ readBytes(m, k), f))
          case Alloc(ack, release, k) ⇒ loop(Bind(() ⇒ doAlloc(ack, release, k), f))
          case Release(key, next)     ⇒ loop(Bind(() ⇒ doRelease(key, next), f))
          case Failure(e)             ⇒ throw reportError(e)
          case Return(a)              ⇒ loop(f(a))
          case Bind(n, g) ⇒
            val next = n().flatMap((x: Any) ⇒ g(x) flatMap f)
            loop(next)
        }
      }

      try loop(p) finally {
        map.foreach {
          case (_, finalizer) ⇒ finalizer()
        }
      }
    }
}
