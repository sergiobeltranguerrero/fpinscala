import fpinscala.exercises.gettingstarted.PolymorphicFunctions.*

def ff(n: Int, s: String): List[String] =
  List.fill(n)(s)
  // List(s * n)

// partial1

// A = Int
// B = String
// C = List[String]
val result: String => List[String] = partial1(5, ff)

result("patata")

// curry

val l = List("patata", "pera", "manzana")

l.map(curry(ff)(2))

// compose

val inc = (a: Int) => a + 1
val repetir = (b: Int) => List.fill(b)("patata")

val f = compose(repetir, inc)
f(3)
