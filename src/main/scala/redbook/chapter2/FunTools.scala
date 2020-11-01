package redbook.chapter2

object FunTools {
  def partial1[A, B , C](a: A, f: (A, B) => C) : B => C = {
    (b: B) => f(a, b)
  }

  def curry[A, B, C](f: (A, B) => C) : A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  def main(args: Array[String]): Unit = {
    println("Testing partial application")
    val add = (a: Int, b: Int) => a + b
    val add2 = partial1(2, add)
    val res = add2(34)
    print("The addition result is %d ".format(res))
    assert(res == 36)

    println("Testing currying")
    val curryingRes = curry(add)(23)(13)
    assert(curryingRes == 36)
    println("curryingRes is %d".format(curryingRes))

    println("Testing Uncurrying")
    val curriedAdd = curry(add)
    val uncurriedAdd = uncurry(curriedAdd)
    val uncurriedRes = uncurriedAdd(12, 24)
    assert(uncurriedRes == 36)
    println("UncurriedRes is %d".format(uncurriedRes))

    println("Testing Composition")
    val doubleFun = (a: Double) => a * 2.00
    val halveFun = (b: Double) => b / 2.00
    val testInput = 45
    val composeResult = compose(doubleFun, halveFun)(testInput)
    println("composeResult is %.2f".format(composeResult))
    assert(testInput == composeResult)
  }
}
