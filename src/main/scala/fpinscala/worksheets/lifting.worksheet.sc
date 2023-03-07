val l = List(1, 0, 2, 1)

def mkpatata(n: Int): String =
  "patata" * n

mkpatata(2)

def lift[A, B](f: A => B): List[A] => List[B] = _.map(f)

l.map(mkpatata)

val mkpatataList = lift(mkpatata)

mkpatataList(List(1, 0, 2, 1))
