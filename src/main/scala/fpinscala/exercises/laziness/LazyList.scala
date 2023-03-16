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

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    (this, that) match
      case (Cons(a, as), Cons(b, bs)) =>
        cons((Some(a()), Some(b())), as().zipAll(bs()))
      case (Cons(a, as), Empty) => cons((Some(a()), None), as().zipAll(that))
      case (Empty, Cons(b, bs)) => cons((None, Some(b())), this.zipAll(bs()))
      case (Empty, Empty)       => empty

  def zipWith[B, C](that: LazyList[B])(f: (A, B) => C): LazyList[C] = ???

  def zip[B](that: LazyList[B]): LazyList[(A, B)] = (this, that) match
    case (Cons(a, as), Cons(b, bs)) => cons((a(), b()), as().zip(bs()))
    case _                          => empty

  def zipWithAll[B, C](that: LazyList[B])(
      f: (Option[A], Option[B]) => C
  ): LazyList[C] = ???

  def zipAllViaZipWithAll[B](
      s2: LazyList[B]
  ): LazyList[(Option[A], Option[B])] = ???

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

  def continually[A](a: A): LazyList[A] = LazyList.cons(a, continually(a))

  def from(n: Int): LazyList[Int] = LazyList.cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] = ???

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case None => empty
      // s es nextState
      case Some((a, s)) => cons(a, unfold(s)(f))

  def unfoldViaMap[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = ???

  def unfoldViaFold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = ???

  // f_0 = 0
  // f_1 = 1
  // f_n = f_(n-1) + f_(n-2)

  // a = 0
  // b = 1
  // i = 0
  // while true do
  //   a, b = b, a + b
  // a es el i-ésimo número de fibonacci

  lazy val fibsViaUnfold: LazyList[Int] =
    unfold((0, 1)) { case (a, b) => Some((a, (b, a + b))) }

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold(n) { s => Some((s, s + 1)) }

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    // () Es de tipo Unit
    unfold(()) { _ => Some((a, ())) }

  lazy val onesViaUnfold: LazyList[Int] = ???
