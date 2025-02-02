package fpinscala.exercises.datastructures

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil

  /** Another data constructor, representing nonempty lists. Note that `tail` is
    * another `List[A]`, which may be `Nil` or another `Cons`.
    */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(
          xs
        ) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1, 2, 3, 4, 5) match
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))

  def foldRight[A, B](
      as: List[A],
      acc: B,
      f: (A, B) => B
  ): B = // Utility functions
    as match
      case Nil         => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x, y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(
      ns,
      1.0,
      _ * _
    ) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // enum List[+A]:
  //   case Nil
  //   case Cons(head: A, tail: List[A])

  // The list w/o the first element
  // Error when empty
  def tail[A](l: List[A]): List[A] = l match
    case Nil           => sys.error("tail of an empty list")
    case Cons(_, tail) => tail

  // Substitute the first element
  // Error when empty
  def setHead[A](l: List[A], h: A): List[A] = l match
    case Nil           => sys.error("tail of an empty list")
    case Cons(_, tail) => Cons(h, tail)

  // The list w/o the first n elements
  // If not enough, return empty
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match
    case Cons(_, tail) if n > 0 => drop(tail, n - 1)
    case _                      => l

  // The list w/o the longest prefix that satisfies f
  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match
    case Cons(head, tail) if f(head) => dropWhile(tail, f)
    case _                           => l

  // The list w/o the last element
  // Error when empty
  // init(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))))
  // Cons(1, init(Cons(2, Cons(3, Cons(4, Nil)))))
  // Cons(1, Cons(2, init(Cons(3, Cons(4, Nil)))))
  // Cons(1, Cons(2, Cons(3, init(Cons(4, Nil)))))
  // Cons(1, Cons(2, Cons(3, Nil))
  def init[A](l: List[A]): List[A] = l match
    case Nil              => sys.error("Empty list not allowed")
    case Cons(_, Nil)     => Nil
    case Cons(head, tail) => Cons(head, init(tail))

  //

  //   case Nil => acc
  //   case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  // Number of elements of list
  // Two versions: direct recursion & fold
  def length[A](l: List[A]): Int = l match
    case Nil           => 0
    case Cons(_, tail) => 1 + length(tail)

  def lengthViaFoldRight[A](l: List[A]): Int =
    foldRight(l, 0, (_, r) => 1 + r)

  // Folding the list from the left
  @annotation.tailrec
  def foldLeft[A, B](l: List[A], acc: B, f: (B, A) => B): B = l match
    case Nil         => acc
    case Cons(x, xs) => foldLeft(xs, f(acc, x), f)

  def sumViaFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double =
    foldLeft(ns, 1.0, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0, (r, _) => 1 + r)

  // Use foldLeft
  // reverse(Cons(1, Cons(2, Cons(3, Nil))))
  // go(Cons(1, Cons(2, Cons(3, Nil))), Nil)
  // go(Cons(2, Cons(3, Nil)), Cons(1, Nil))
  // go(Cons(3, Nil), Cons(2, Cons(1, Nil)))
  // go(Nil, Cons(3, Cons(2, Cons(1, Nil))))
  // Cons(3, Cons(2, Cons(1, Nil)))
  def reverseTailRec[A](l: List[A]): List[A] =
    @annotation.tailrec
    def go(l: List[A], acc: List[A]): List[A] =
      l match
        case Nil              => acc
        case Cons(head, tail) => go(tail, Cons(head, acc))
    go(l, Nil)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A], (rcr, a) => Cons(a, rcr))

  // Implement foldRightViaFoldLeft
  def foldRightViaFoldLeft[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverse(as), acc, (b, a) => f(a, b))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r, Cons(_, _))
    // foldRight(l, r, (a: A, l: List[A]) => Cons(a, l))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, List[A](), append)

  def incrementEach(l: List[Int]): List[Int] = l match
    case Nil         => Nil
    case Cons(x, xs) => Cons(x + 1, incrementEach(xs))

  def incrementEachViaFoldRight(l: List[Int]): List[Int] =
    // r es la llista de Ints ja incrementada.
    foldRight(l, List[Int](), (a, r) => Cons(a + 1, r))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, List[String](), (a, r) => Cons(a.toString(), r))

  def map[A, B](l: List[A], f: A => B): List[B] = l match
    case Nil         => Nil
    case Cons(x, xs) => Cons(f(x), map(xs, f))

    // foldRight(l, List[B](), (a, r) => Cons(f(a), r))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    foldRight(as, List[A](), (a, r) => if f(a) then Cons(a, r) else r)

  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] =
    foldRight(as, List[B](), (a, r) => append(f(a), r))
    // concat(map(as, f))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, a => if f(a) then List(a) else List()) // f: A => List[A]

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addPairwise(xs, ys))
    case (_, _)                     => Nil

  // def zipWith - TODO determine signature

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???
