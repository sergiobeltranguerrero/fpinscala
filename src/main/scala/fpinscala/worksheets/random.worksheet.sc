import fpinscala.exercises.state.*

val rng = RNG.Simple(42L)

rng.nextInt

val ints1 = RNG.ints(5)(rng)
val ints2 = RNG.intsTR(5)(rng)
