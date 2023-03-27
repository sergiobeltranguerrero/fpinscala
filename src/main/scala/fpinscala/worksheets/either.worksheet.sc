import fpinscala.exercises.errorhandling.Either
import fpinscala.exercises.errorhandling.Either.*

def right[E]: [A] => A => Either[E, A] =
  [A] => (a: A) => Right(a)

right[String](List.empty[Int])
