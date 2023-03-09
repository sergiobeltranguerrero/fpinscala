def maybeTwice2(b: Boolean, i: => Int) =
  lazy val j = i
  if b then j + j else 0

def maybeTwice3(b: Boolean, i: => Int) =
  if b then
    val j = i
    j + j
  else 0

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def headOption: Option[A] = this match
    case Empty      => None
    case Cons(h, t) => Some(h())

def f(n: Int): Int =
  println(s"computing $n")
  n

import LazyList.*

val lazy_ = Cons(() => f(1), () => Cons(() => f(2), () => Empty))

val strict = List(f(1), f(2))

lazy_.headOption

lazy_.headOption
