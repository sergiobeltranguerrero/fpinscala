package fpinscala.exercises.errorhandling

// Hide std library `Either` since we are writing our own in this chapter
import scala.{Either as _, Left as _, Right as _}
import scala.util.control.NonFatal
import fpinscala.answers.applicative.Traverse.Iteration.a

enum Either[+E, +A]:
  case Left(get: E)
  case Right(get: A)

  def map[B](f: A => B): Either[E, B] = this match
    case Left(e)  => Left(e)
    case Right(a) => Right(f(a))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match
    case Left(e)  => Left(e)
    case Right(a) => f(a)

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match
    case Left(_)  => b
    case Right(a) => this

  def map2[EE >: E, B, C](eb: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for
      a <- this
      b <- eb
    yield f(a, b)

    // this match
    //   case Left(e) => Left(e)
    //   case Right(a) =>
    //     b match
    //       case Left(ee) => Left(ee)
    //       case Right(b) => Right(f(a, b))

object Either:

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right(Nil): Either[E, List[B]]) { (a, acc) =>
      f(a).map2(acc)(_ :: _)
    }

  def identity2[A](a: A): A = a

  // List[Either[F, C]] <-> List[A]
  // A = Either[F, C]
  // f: Either[F, C] => Either[E, B]
  // identity: Either[F, C] => Either[F, C]
  // E = F
  // B = C
  // Either[E, List[B]] = Either[F, List[C]]
  def sequence[F, C](es: List[Either[F, C]]): Either[F, List[C]] =
    traverse(es)(identity)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if xs.isEmpty then Left("mean of empty list!")
    else Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Throwable, Int] =
    try Right(x / y)
    catch case NonFatal(t) => Left(t)

  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  def map2All[E, A, B, C](
      a: Either[List[E], A],
      b: Either[List[E], B],
      f: (A, B) => C
  ): Either[List[E], C] = ???

  def traverseAll[E, A, B](
      as: List[A],
      f: A => Either[List[E], B]
  ): Either[List[E], List[B]] = ???

  def sequenceAll[E, A](
      as: List[Either[List[E], A]]
  ): Either[List[E], List[A]] = ???
