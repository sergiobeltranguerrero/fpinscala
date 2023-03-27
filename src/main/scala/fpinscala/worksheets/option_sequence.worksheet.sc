import fpinscala.exercises.errorhandling.Option
import fpinscala.exercises.errorhandling.Option.*

val l = List(Some(1), Some(2), Some(3))

Option.sequence(l)

// def map     [A, B](as: List[A])(f: A =>        B) : List[B]
// def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]]

def onlyPositives(n: Int): Option[String] =
  if n > 0 then Some(n.toString)
  else None

val data = List(1, 2, -3, 4, -5)

data.map(onlyPositives)

Option.traverse(data)(onlyPositives)

Option
  .sequence(data.map(onlyPositives).filter {
    case Some(_) => true
    case None    => false
  })
  .getOrElse(Nil)
