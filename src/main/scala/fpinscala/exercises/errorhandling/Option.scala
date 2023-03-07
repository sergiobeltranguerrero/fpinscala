package fpinscala.exercises.errorhandling

// Hide std library `Option` since we are writing our own in this chapter
import scala.{Option as _, Some as _, None as _}

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = this match
    case None    => None
    case Some(a) => Some(f(a))

  def flatMap[B](f: A => Option[B]): Option[B] = this match
    case None    => None
    case Some(a) => f(a)

  def fold[B](ifNone: => B, ifSome: A => B): B = this match
    case None    => ifNone
    case Some(a) => ifSome(a)

  def filter(f: A => Boolean): Option[A] = this match
    case sa @ Some(a) if f(a) => sa
    case _                    => None

  def getOrElse[B >: A](default: => B): B = this match
    case Some(a) => a
    case None    => default

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match
    case Some(_) => this
    case None    => ob

  def flatMapGOE[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None)

  def filterFM(f: A => Boolean): Option[A] =
    flatMap(a => if f(a) then Some(a) else None)

  def orElseGOE[B >: A](ob: => Option[B]): Option[B] =
    this.map(Some(_)).getOrElse(ob)

object Option:

  def failingFn(i: Int): Int =
    val y: Int =
      throw new Exception(
        "fail!"
      ) // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try
      val x = 42 + 5
      x + y
    catch
      case e: Exception =>
        43 // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.

  def failingFn2(i: Int): Int =
    try
      val x = 42 + 5
      x + ((throw new Exception(
        "fail!"
      )): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    catch case e: Exception => 43

  def mean(xs: List[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  def variance(xs: List[Double]): Option[Double] =
    // If mean signaled errors via exceptions the code would have been:
    // val m = mean(xs)
    // val ts = xs.map(x => math.pow(x - m, 2))
    // mean(ts)
    mean(xs).flatMap { m =>
      val ts = xs.map(x => math.pow(x - m, 2))
      mean(ts)
    }

  def map2[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    (oa, ob) match
      case (Some(a), Some(b)) => Some(f(a, b))
      case _                  => None

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???

  def parseInsuranceRateQuote(
      age: String,
      numberOfSpeedingTickets: String
  ): Option[Double] =
    // val optAge: Int = Integer.parseInt(age)
    // val optTickets: Int = Integer.parseInt(numberOfSpeedingTickets)
    // insuranceRateQuote(age, tickets)
    val optAge: Option[Int] = ???
    val optTickets: Option[Int] = ???
    // optAge.flatMap { age =>
    //   optTickets.map { tickets =>
    //     insuranceRateQuote(age, tickets)
    //   }
    // }
    for
      age <- optAge
      tickets <- optTickets
    yield insuranceRateQuote(age, tickets)

  def sequence[A](as: List[Option[A]]): Option[List[A]] = as match
    case Nil       => Some(Nil)
    case oa :: oas => map2(oa, sequence(oas))(_ :: _)
    // for
    //   a <- oa
    //   as <- sequence(oas)
    // yield a :: as

    // oa: Option[A]
    // oas: List[Option[A]] ----> Option[List[A]]
    // ---> Option[List[A]]
    // ----------> List[A]
    // -------------------> Nil
    // -------------------> a: A as: List[A]  ----> a :: as

//def map     [A, B](as: List[A])(f: A =>        B) : List[B]
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match
    case Nil     => Some(Nil)
    case a :: as => map2(f(a), traverse(as)(f))(_ :: _)
