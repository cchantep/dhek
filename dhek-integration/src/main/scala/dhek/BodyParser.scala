package dhek

import java.io.{
  ByteArrayOutputStream,
  InputStream,
  IOException,
  PushbackInputStream
}

import scalaz.{ Free, Functor }
import scalaz.Free.{ Return, Suspend }

import resource.managed

object BodyParser {

  type Parser[A] = Free[Instr, A]
  type ReleaseKey = Int

  trait Instr[+A] {
    def map[B](f: A ⇒ B): Instr[B] = this match {
      case r @ Get(k)         ⇒ r.copy(k = f compose k)
      case r @ Peek(k)        ⇒ r.copy(k = f compose k)
      case r @ Take(_, k)     ⇒ r.copy(k = f compose k)
      case r @ Buffer(_, k)   ⇒ r.copy(k = f compose k)
      case r @ OrElse(x, y)   ⇒ r.copy(l = f(x), r = f(y))
      case r @ Commit(n)      ⇒ r.copy(n = f(n))
      case r @ Alloc(_, _, _) ⇒ r.mapping(f)
      case r @ Release(_, n)  ⇒ r.copy(next = f(n))
      case Failure(e)         ⇒ Failure(e)
    }
  }

  case class Get[A](k: Byte ⇒ A) extends Instr[A]
  case class Peek[A](k: Byte ⇒ A) extends Instr[A]
  case class Take[A](n: Int, k: Array[Byte] ⇒ A) extends Instr[A]
  case class Buffer[A](max: Option[Int], k: Array[Byte] ⇒ A) extends Instr[A]
  case class OrElse[A](l: A, r: A) extends Instr[A]
  case class Commit[A](n: A) extends Instr[A]
  case class Alloc[A, B](ack: () ⇒ A, release: A ⇒ Unit, k: (A, ReleaseKey) ⇒ B) extends Instr[B] {
    def mapping[C](f: B ⇒ C): Alloc[A, C] =
      copy(k = (a: A, r: ReleaseKey) ⇒ f(k(a, r)))
  }
  case class Release[A](key: ReleaseKey, next: A) extends Instr[A]
  case class Failure(e: Either[String, Throwable]) extends Instr[Nothing]

  object Instr {
    implicit val instrFunctor = new Functor[Instr] {
      def map[A, B](fa: Instr[A])(f: A ⇒ B): Instr[B] = fa map f
    }
  }

  def unit: Parser[Unit] =
    Return(())

  def getByte: Parser[Byte] =
    Suspend(Get(Return(_)))

  def peekByte: Parser[Byte] =
    Suspend(Peek(Return(_)))

  def take(n: Int): Parser[Array[Byte]] =
    Suspend(Take(n, Return(_)))

  val integer: Parser[Int] = for {
    bs ← takeWhile1(b ⇒ Character.isDigit(b.toChar))
    i ← beware(new String(bs).toInt)
  } yield i

  def liftString(bs: Array[Byte]): Parser[String] =
    beware(new String(bs))

  def buffer(max: Option[Int] = Some(8192)): Parser[Array[Byte]] =
    Suspend(Buffer(max, Return(_)))

  def alloc[R](ack: ⇒ R, release: R ⇒ Unit): Parser[(R, ReleaseKey)] =
    Suspend[Instr, (R, ReleaseKey)](Alloc[R, Free[Instr, (R, ReleaseKey)]](
      () ⇒ ack,
      release,
      (r, key) ⇒ Return((r, key))))

  def release(key: ReleaseKey): Parser[Unit] =
    Suspend(Release(key, Return(())))

  def takeWhile1(p: Byte ⇒ Boolean): Parser[Array[Byte]] =
    for {
      b ← getByte
      _ ← if (p(b)) unit else failure("takeWhile1: predicate not satisfied")
      bs ← takeWhile(p)
    } yield {
      val buffer = new ByteArrayOutputStream()

      buffer.write(b)
      buffer.write(bs)

      buffer.toByteArray
    }

  def takeWhile(p: Byte ⇒ Boolean): Parser[Array[Byte]] = {
    val buffer = new ByteArrayOutputStream()

    def shift: Parser[Unit] =
      getByte.map(b ⇒ buffer.write(b))

    def loop: Parser[Array[Byte]] =
      for {
        b ← peekByte
        bs ← if (p(b)) shift.flatMap(_ ⇒ loop) else Return[Instr, Array[Byte]](buffer.toByteArray)
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

  def optional[A](p: Parser[A]): Parser[Option[A]] = {
    val action = for {
      a ← p
      r ← commit(Some(a))
    } yield r

    Suspend(OrElse(action, Return(None)))
  }

  def failure(msg: String): Parser[Nothing] =
    Suspend(Failure(Left(msg)))

  def throwable(e: Throwable): Parser[Nothing] =
    Suspend(Failure(Right(e)))

  def beware[A](v: ⇒ A): Parser[A] =
    try Return(v) catch {
      case e: Throwable ⇒ throwable(e)
    }

  private def commit[A](a: A): Parser[A] =
    Suspend(Commit(Return(a)))

  private lazy val unexpectedEOF: Either[Throwable, Nothing] =
    Left(new RuntimeException("End of Stream"))

  private lazy val emptyBuffer: Array[Byte] = Array()

  def runParser[A](p: Parser[A], input: ⇒ InputStream, bsize: Int = 8192): Either[List[Throwable], A] =
    managed(new PushbackInputStream(input, bsize)).acquireFor { s ⇒
      type Finalizers = scala.collection.mutable.Map[Int, () ⇒ Unit]
      type Track = Option[ByteArrayOutputStream]
      type Tracks = List[ByteArrayOutputStream]
      type Alternatives = List[Parser[A]]

      val buffer = new Array[Byte](bsize)
      var keyCnt = 0

      def readByte(k: Byte ⇒ Parser[A], track: Track): Either[Throwable, Parser[A]] = {
        val b = s.read

        if (b == -1)
          unexpectedEOF
        else {
          track foreach { _ write b }

          Right(k(b.toByte))
        }
      }

      def peekByte(k: Byte ⇒ Parser[A]): Either[Throwable, Parser[A]] = {
        val b = s.read

        if (b == -1)
          unexpectedEOF
        else {
          s.unread(b)

          Right(k(b.toByte))
        }
      }

      def takeBytes(n: Int, k: Array[Byte] ⇒ Parser[A], track: Track): Either[Throwable, Parser[A]] =
        if (n <= bsize) {
          val readByte = s.read(buffer, 0, n)
          lazy val slice = buffer.slice(0, n)

          if (readByte != -1)
            track foreach { _ write slice }

          if (readByte < n)
            unexpectedEOF
          else
            Right(k(slice))
        } else {
          val tmpBuffer = new Array[Byte](n)
          val readByte = s.read(tmpBuffer)

          if (readByte != -1)
            track foreach { _ write tmpBuffer.slice(0, n) }

          if (readByte < n)
            unexpectedEOF
          else
            Right(k(tmpBuffer))
        }

      def readBytes(max: Option[Int], k: Array[Byte] ⇒ Parser[A], track: Track): Parser[A] = {
        val tmpBuffer = max.fold(buffer) {
          case limit if limit <= bsize ⇒ buffer
          case limit                   ⇒ new Array[Byte](limit)
        }

        val readBytes = s.read(tmpBuffer, 0, bsize)
        lazy val slice = tmpBuffer.slice(0, readBytes)

        if (readBytes != -1)
          track foreach { _ write slice }

        if (readBytes == -1)
          k(emptyBuffer)
        else
          k(slice)
      }

      def doAlloc(
        map: Finalizers,
        ack: () ⇒ Any,
        release: Any ⇒ Unit,
        k: (Any, ReleaseKey) ⇒ Parser[A]): Parser[A] = {

        val r = ack()
        val curKey = keyCnt
        map += (keyCnt, () ⇒ release(r))
        keyCnt += 1
        k(r, curKey)
      }

      def doRelease(map: Finalizers, key: ReleaseKey, next: Parser[A]): Parser[A] = {
        map.get(key).foreach { finalizer ⇒
          finalizer()
          map - key
        }

        next
      }

      case class Next(tracks: Tracks, alts: Alternatives, next: Parser[A])

      def handleError(
        tracks: Tracks,
        alts: Alternatives,
        e: Throwable): Either[Throwable, Next] = (tracks, alts) match {

        case (t :: ts, a :: as) ⇒
          s.unread(t.toByteArray)
          Right(Next(ts, as, a))
        case _ ⇒
          println((tracks, alts))
          Left(e)
      }

      def newTrack = new ByteArrayOutputStream()

      def reportError(e: Either[String, Throwable]): Throwable =
        e.fold(m ⇒ new IOException(m), identity)

      case class State(tracks: Tracks, alts: Alternatives, map: Finalizers)

      val map = scala.collection.mutable.Map[Int, () ⇒ Unit]()
      lazy val res = p.foldRun(State(Nil, Nil, map)) {
        case (st, Get(k)) ⇒ readByte(k, st.tracks.headOption) match {
          case Left(e) ⇒ handleError(st.tracks, st.alts, e) match {
            case Left(e) ⇒ throw e
            case Right(Next(ntracks, nalts, next)) ⇒
              (st.copy(tracks = ntracks, alts = nalts), next)
          }
          case Right(next) ⇒ (st, next)
        }
        case (st, Peek(k)) ⇒ peekByte(k) match {
          case Left(e) ⇒ handleError(st.tracks, st.alts, e) match {
            case Left(e) ⇒ throw e
            case Right(Next(ntracks, nalts, next)) ⇒
              (st.copy(tracks = ntracks, alts = nalts), next)
          }
          case Right(next) ⇒ (st, next)
        }
        case (st, Take(n, k)) ⇒ takeBytes(n, k, st.tracks.headOption) match {
          case Left(e) ⇒ handleError(st.tracks, st.alts, e) match {
            case Left(e) ⇒ throw e
            case Right(Next(ntracks, nalts, next)) ⇒
              (st.copy(tracks = ntracks, alts = nalts), next)
          }
          case Right(next) ⇒ (st, next)
        }
        case (st, Buffer(m, k)) ⇒ (st, readBytes(m, k, st.tracks.headOption))
        case (st, OrElse(l, r)) ⇒
          (st.copy(tracks = newTrack :: st.tracks, alts = r :: st.alts), l)
        case (State(_ :: ts, _ :: as, map), Commit(next)) ⇒
          (State(ts, as, map), next)
        case (st, Alloc(ack, release, k)) ⇒
          (st, doAlloc(st.map, ack, release, k))
        case (st, Release(key, next)) ⇒ (st, doRelease(st.map, key, next))
        case (st, Failure(e)) ⇒
          handleError(st.tracks, st.alts, reportError(e)) match {
            case Left(e) ⇒ throw e
            case Right(Next(ntracks, nalts, next)) ⇒
              (st.copy(tracks = ntracks, alts = nalts), next)
          }
      }

      try res._2 finally {
        map.foreach {
          case (_, finalizer) ⇒ finalizer()
        }
      }
    }
}
