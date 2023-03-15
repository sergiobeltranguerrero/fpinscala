package fpinscala.exercises.laziness

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  import LazyList.*

  def toList: List[A] = this match
    case Empty      => Nil
    case Cons(h, t) => h() :: t().toList

  // The arrow `=>` in front of an argument type means that the function `f` takes this argument by name
  // and may choose not to evaluate it.
  def foldRight[B](z: => B)(f: (=> A, => B) => B): B =
    this match
      case Cons(h, t) =>
        f(
          h(),
          t().foldRight(z)(f)
        ) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) =>
      p(a) || b
    ) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _                   => empty

  def drop(n: Int): LazyList[A] = this match
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _                   => this

  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _                    => empty

  // def foldRight[B](z: => B)(f: (=> A, => B) => B): B =
  def forAll(p: A => Boolean): Boolean =
    this.foldRight(true)((a, r) => p(a) && r)

  def takeWhile2(p: A => Boolean): LazyList[A] =
    this.foldRight(empty) { (a, r) =>
      if p(a) then cons(a, r) else empty
    }

  def headOption: Option[A] = this match
    case Empty      => None
    case Cons(h, t) => Some(h())

  def headOption2: Option[A] =
    this.foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): LazyList[B] =
    this.foldRight(empty) { (a, r) =>
      // a: primer element de this
      // r: resultat del map a la resta de la LazyList this
      // -> el map a tota la LazyList this
      cons(f(a), r)
    }

  def filter(p: A => Boolean): LazyList[A] =
    this.foldRight(empty) { (a, r) =>
      // a: primer element de this
      // r: resultat del filter then a la resta de la LazyList this
      if p(a) then cons(a, r) else r
    }

  def append[A2 >: A](that: => LazyList[A2]): LazyList[A2] =
    this.foldRight(that)(cons)

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    this.foldRight(empty) { (a, r) =>
      f(a).append(r)
    }

  def startsWith[B](s: LazyList[B]): Boolean = ???

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = ???

  def from(n: Int): LazyList[Int] = ???

  lazy val fibs: LazyList[Int] = ???

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = ???

  lazy val fibsViaUnfold: LazyList[Int] = ???

  def fromViaUnfold(n: Int): LazyList[Int] = ???

  def continuallyViaUnfold[A](a: A): LazyList[A] = ???

  lazy val onesViaUnfold: LazyList[Int] = ???
