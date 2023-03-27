def debug(n: Int): Int = { println(s"Computing $n"); n }

def fNone(a: => Int, b: => Int, g: (Int, Int) => Int): Int = g(a, b)
fNone(debug(1), debug(2), (a, b) => a + b)
fNone(debug(1), debug(2), (a, _) => a)
fNone(debug(1), debug(2), (_, b) => b)
fNone(debug(1), debug(2), (_, _) => 42)

def fFirst(a: => Int, b: => Int, g: (=> Int, Int) => Int): Int = g(a, b)
fFirst(debug(1), debug(2), (a, b) => a + b)
fFirst(debug(1), debug(2), (a, _) => a)
fFirst(debug(1), debug(2), (_, b) => b)
fFirst(debug(1), debug(2), (_, _) => 42)

def fSecond(a: => Int, b: => Int, g: (Int, => Int) => Int): Int = g(a, b)
fSecond(debug(1), debug(2), (a, b) => a + b)
fSecond(debug(1), debug(2), (a, _) => a)
fSecond(debug(1), debug(2), (_, b) => b)
fSecond(debug(1), debug(2), (_, _) => 42)

def fAll(a: => Int, b: => Int, g: (=> Int, => Int) => Int): Int = g(a, b)
fAll(debug(1), debug(2), (a, b) => a + b)
fAll(debug(1), debug(2), (a, _) => a)
fAll(debug(1), debug(2), (_, b) => b)
fAll(debug(1), debug(2), (_, _) => 42)

val f: (=> Int, => Int) => Int = (_, _) => 14
// fNone(debug(2), debug(4), f)
// fFirst(debug(2), debug(4), f)
// fSecond(debug(2), debug(4), f)
fAll(debug(2), debug(4), f)

// val g: (=> Int, Int) => Int = f
val g: (=> Int, Int) => Int = (a, b) => f(a, b)
// fNone(debug(2), debug(4), g)
fFirst(debug(2), debug(4), g)
// fSecond(debug(2), debug(4), g)
// fAll(debug(2), debug(4), g)
