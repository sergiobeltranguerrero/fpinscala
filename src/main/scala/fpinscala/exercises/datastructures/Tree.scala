package fpinscala.exercises.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match
    case Leaf(_)             => 1
    case Branch(left, right) => 1 + math.max(left.depth, right.depth)

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(value)         => Leaf(f(value))
    case Branch(left, right) => Branch(left.map(f), right.map(f))

  def fold[B](f: A => B, g: (B, B) => B): B = this match
    case Leaf(value)         => f(value)
    case Branch(left, right) => g(left.fold(f, g), right.fold(f, g))

  def sizeViaFold: Int =
    this.fold(_ => 1, _ + _)
    /*
      this match
        case Leaf(value) => (_ => 1)(value)
        case Branch(left, right) => (_ + _)(left.fold(_ => 1, _ + _), right.fold(_ => 1, _ + _))

      this match
        case Leaf(value) => 1
        case Branch(left, right) => left.fold(_ => 1, _ + _) + right.fold(_ => 1, _ + _)
     */

  def depthViaFold: Int =
    this.fold(_ => 1, 1 + math.max(_, _))

  def mapViaFold[B](f: A => B): Tree[B] =
    this.fold(a => Leaf(f(a)), Branch(_, _))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + size(l) + size(r)

  extension (t: Tree[Int])
    // Returns the first positive value found or -1 if it does not exist
    def firstPositive: Int = t match
      case Leaf(value) => if value < 1 then -1 else value
      case Branch(left, right) =>
        val firstPositiveLeft = left.firstPositive
        if firstPositiveLeft > 0
        then firstPositiveLeft
        else right.firstPositive

  extension (t: Tree[Int])
    def maximum: Int = t match
      case Leaf(value)         => value
      case Branch(left, right) => math.max(left.maximum, right.maximum)

  extension (t: Tree[Int])
    def maximumViaFold: Int =
      t.fold(identity, math.max)
