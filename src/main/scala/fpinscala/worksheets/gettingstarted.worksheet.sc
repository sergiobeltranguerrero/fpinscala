def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
  (b: B) => f(a, b)

def ff(n: Int, s: String): List[String] =
  List.fill(n)(s)
  // List(s * n)

// A = Int
// B = String
// C = List[String]
val result: String => List[String] = partial1(5, ff)

result("patata")

def curry[A, B, C](f: (A, B) => C): A => (B => C) =
  (a: A) => (b: B) => f(a, b)

val l = List("patata", "pera", "manzana")

l.map(curry(ff)(2))

def compose[A, B, C](f: B => C, g: A => B): A => C =
  a => f(g(a))

val inc = (a: Int) => a + 1

val repetir = (b: Int) => List.fill(b)("patata")

val f = compose(repetir, inc)

f(3)
