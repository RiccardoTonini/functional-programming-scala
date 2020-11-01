package redbook.chapter2

object MyModule {
  def abs(n: Int): Int =
    if (n > 0) n else -n

  private def formatAbs(n: Int) = {
    val msg = "Absolute value of %d is %d"
    msg.format(n, abs(n))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d"
    msg.format(n, factorial(n))
  }

  private def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %d of %d is %d"
    msg.format(name, n, f(n))
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)

    go(n, acc=1)
  }

  def fib(n: Int): Int = {
    if (n <= 1) n
    else fib(n - 1) + fib(n-2)
  }

  def fib2(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, previous: Int, current: Int): Int = {
      if (n <= 0) current
      else go(n-1, previous=previous+current, current=previous)
    }
    go(n, previous=1, current=0)
  }

  def findFirst[A] (as: Array[A], p: A => Boolean): Int = {
    def go(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else go(n + 1)
    }

    go(0)
  }

  def isSorted[A] (as: Array[A], ordered:(A, A) => Boolean): Boolean = {
    def go(index: Int, sortedSoFar: Boolean): Boolean = {
      val nextIndex = index + 1
      // Termination case: we've iterated through the array
      if (nextIndex >= as.length) sortedSoFar
      // Base case: we found an unsorted pair, we might as well return early
      else if (index > 0 && sortedSoFar == false) sortedSoFar
      else go(index + 1, ordered(as(index), as(nextIndex)))
    }
    go(0, false)
  }

  def isSorted2[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int, acc: Boolean): Boolean = {
      if ((n + 1) >= as.length)
        acc
      else
        loop(n + 1, acc && ordered(as(n), as(n + 1)))
    }

    loop(0, true)
  }

  def main(args: Array[String]) = {
    val test = Array("one", "two", "three")
    val findOne = (x: Any) => x == "search"
    val i = findFirst(test, findOne)
    val msg = "The index of %s is %d"
    println(msg.format("\"one\"", i))

    val lessOrEqualThan = (x: Int, y: Int) => x <= y
    val sortedList = Array(1, 2, 3, 4)
    val testASortedList = isSorted(sortedList, lessOrEqualThan)
    println("testASortedList is %b".format(testASortedList))
    val unsortedList = Array(3, 1, 2, 3, 4)
    val testUnsortedList = isSorted(unsortedList, lessOrEqualThan)
    println("testUnsortedList is %b".format(testUnsortedList))

  }
}
