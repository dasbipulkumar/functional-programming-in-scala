
// Exercise 2.1
// Tail-recursive fibonacci
def fib(n: Int): Int = {
  @annotation.tailrec
  def go(n: Int, a: Int, b: Int): Int = n match {
    case 0 => a
    case _ => go(n - 1, b, a + b)
  }

  go(n, 0, 1)
}

// Exercise 2.2
// Generic implementation of isSorted
def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
  @annotation.tailrec
  def loop(n: Int): Boolean = {
    if (n == as.length - 1) true
    else if (!ordered(as(n), as(n + 1))) false
    else loop(n + 1)
  }
  loop(0)
}

// Partial Application
def partial1[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

// Exercise 2.3
// Currying
def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)

// Exercise 2.4
// Uncurry
def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

// Exercise 2.5
// Function Composition
def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

