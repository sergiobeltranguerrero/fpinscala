val l1 = List(1, 2, 3)
val l2 = List("a", "b")

for // se puede ver como un flatmap y map
  n <- l1
  s <- l2
yield (n, s)

l1.flatMap { n =>
  l2.map { s =>
    (n, s)
  }
}
