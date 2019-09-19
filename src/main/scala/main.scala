import scalaz._
import Scalaz._
import simulacrum._

import scala.concurrent.Future
object main {
  trait Foo[C[_]] {
    def create(i: Int): C[Int]
  }

  object FooList extends Foo[List] {
    override def create(i: Int): List[Int] = List(i)
  }

  type EitherString[T] = Either[String, T]
  type Id[T]           = T

  object FooEitherString extends Foo[EitherString] {
    override def create(i: Int): Either[String, Int] = Right(i)
  }

  object FooId extends Foo[Id] {
    def create(i: Int): Int = i
  }

  trait Terminal[C[_]] {
    def read: C[String]
    def write(t: String): C[Unit]
  }

  trait Execution[C[_]] {
    def chain[A, B](c: C[A])(f: A => C[B]): C[B]
    def create[B](b: B): C[B]
  }

  object Execution {
    implicit class Ops[A, C[_]](c: C[A]) {
      def flatMap[B](f: A => C[B])(implicit e: Execution[C]): C[B] =
        e.chain(c)(f)
      def map[B](f: A => B)(implicit e: Execution[C]): C[B] =
        e.chain(c)(f andThen e.create)
    }
  }

  def echo[C[_]](implicit t: Terminal[C], e: Execution[C]): C[String] =
    t.read.flatMap { in: String =>
      t.write(in).map { _: Unit =>
        in
      }
    }

  type Now[X] = X

  object TerminalSync extends Terminal[Now] {
    override def read: Now[String]           = ???
    override def write(t: String): Now[Unit] = ???
  }

  object TerminalAsync extends Terminal[Future] {
    override def read: Future[String]           = ???
    override def write(t: String): Future[Unit] = ???
  }

  final class IO[A](val interpret: () => A) {
    def map[B](f: A => B): IO[B]         = IO(f(interpret()))
    def flapMap[B](f: A => IO[B]): IO[B] = IO(f(interpret()).interpret())
  }

  object IO {
    def apply[A](a: => A): IO[A] = new IO(() => a)
  }

  object TerminalIO extends Terminal[IO] {
    override def read: IO[String]           = IO { io.StdIn.readLine() }
    override def write(t: String): IO[Unit] = IO { println(t) }
    val delayed: IO[String]                 = echo[IO]
    delayed.interpret
  }

}
