package redbook.chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

object RandomGenerator {
  /**
   * A type alias that models a randomly-generated A.
   * It represents a state action or state transition,
   * i.e. program (function) that
   * depends on some RNG, uses it to generate an A,
   * and transitions the state of RNG to a new state that
   * can be used by the next action.
   * @tparam A
   */
  type Rand[+A] = RNG => (A, RNG)

  /**
   * We can now turn methods such as RNG's nextInt()
   * into values of this new type
   */
  val int: Rand[Int] = _.nextInt

  /**
   * We can use combinators to combine Rand transitions without
   * having to pass the RNG state along.
   * The unit action is a simple RNG state transition, which passes
   * the RNG state through without using it, always returning a constant
   * value instead of a random one.
   * @param a
   * @tparam A
   * @return
   */
  def unit[A](a: A): Rand[A] = {
    rng => (a, rng)
  }

  def map[A, B](transition: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, nextRNG) = transition(rng)
      (f(a), nextRNG)
    }
  }

  /**
   * Un fortunately map is not powerful enough to implement intDouble
   * and doubleInt (6.3). We need a combinator map2 that can combine
   * 2 RNG transitions into 1 by means of a binary rather than a unary
   * function.
    * This function takes 2 actions (ra and rb) and a function f for
   * combining their results, and returns a new state transition that
   * combines them.
   */
  def map2[A, B, C](
    transitionStateA: Rand[A], transitionStateB: Rand[B]
  )(f: (A, B) => C): Rand[C] = {
    rng =>  {
      val (a, nextRNGA) = transitionStateA(rng)
      val (b, nextRNGB) = transitionStateB(nextRNGA)
      (f(a, b), nextRNGB)
    }
  }

  /**
   * We can use map2 to combine arbitrary RNG state transitions.
   * For example, if we have a transition that generates values of
   * type A, and another values of type B, we can then combine them
   * into 1 action that generates pairs of both A and B
   * @param ra
   * @param rb
   * @tparam A
   * @tparam B
   * @return
   */
  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = {
    // Underscore: match positional arg in anonymous function
    // This makes the code concise; though less explicit
    map2(ra, rb){(_, _)}
  }

  def randIntDouble2: Rand[(Int, Double)] =
    both(int, double)

  def randDoubleInt2: Rand[(Double, Int)] =
    both(double, int)

  /**
   * 6.7
   * If you can combine 2 RNG transitions, you should be able to
   * combine a whole list of them. Implement sequence() for combining
   * a List of transitions into a single transition.
   * In sequence(), the base case of the fold is a unit action that
   * returns the empty list.
   * At each step in the fold, we accumulate in accumulator and
   * f is the current element in the list.
   * map2(transition, transitionsSoFar)(_ :: _) results in a value of type Rand[List[A]]
   * We map over that to prepend (cons) the element onto the accumulated
   * list.
   *
   * @param transitions
   * @tparam A
   * @return
   */
  def sequence[A](transitions: List[Rand[A]]): Rand[List[A]] = {
    val emptyTransitions = List()
    transitions.foldRight(
      unit(emptyTransitions)
    ){
      (transition, transitionsSoFar) => map2(transition, transitionsSoFar)(_ :: _)
      //(transition, transitionsSoFar) => map2(transition, transitionsSoFar){
      //  (transition, transitionsSoFar) => transition :: transitionsSoFar
      //}
    }

  }

  /**
   * 6.7
   * Use sequence and List.fill to reimplement the ints() function
   * @param count
   * @param rng
   * @return
   */
  def intsSequence(count: Int)(rng: SimpleRNG): Rand[List[Int]] = {
    // Question why with type hint SimpleRNG does not work? -> curly braces {}
    val transitions: List[Rand[Int]] = List.fill(count){rng:RNG => rng.nextInt}
    sequence(transitions)
  }

  def nonNegativeLessThan1(n: Int): Rand[Int] = {
    map(nonNegativeInt){ _ % n }
  }

  /**
   * Recursive implementation of nonNegativeLessThan
   * @param n
   * @return
   */
  def nonNegativeLessThan2(n: Int): Rand[Int] = {rng =>
    val (i, nextRNG) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1 ) - mod >= 0)
      (mod, nextRNG)
    else nonNegativeLessThan2(n)(rng)
  }

  /**
   * Clarify syntax for code block
   * @param transition
   * @param g
   * @tparam A
   * @tparam B
   * @return
   */
  def flatMap[A, B](transition: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, nextRNG) = transition(rng)  // Flatten
      g(a)(nextRNG)  // Map to Rand[B] which is function that takes a RNG and returns a (A, RNG)
    }
  }

  case class SimpleRNG(seed: Long) extends RNG {

    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }

    /**
     * Example of map used to generate a Int that's
     * greater than or equal to 0 and divisible by 2.
     * @return
     */
    def nonNegativeEven: Rand[Int] = {
      map(nonNegativeInt)(i => i - i % 2)
    }
  }

  /**
   * Write a function that uses RNG.nextInt to generate a random
   * int between 0 and Int.maxValue (inclusive)
   * Handle corner case when RNG.nextInt() returns Int.MinValue,
   * which does not have a negative counterpart.
   * Int.Minvalue is smaller than -(Int.MaxValue) by 1,
   * so we can increment the negative numbers by 1
   * and make them positive
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, nextRng) = rng.nextInt
    if (i < 0) {
      (-(i + 1), nextRng)
    } else {
      (i, nextRng)
    }
  }

  /**
   * Write a function to generate a Double between 0 and 1, not including 1.
   * You can use Int.MaxValue to obtain the max positive int value, and
   * toDouble to convert an Int to Double
   * int between 0 and Int.maxValue (inclusive)
   * We generate a positive integer and divide it
   * by 1 higher than the maximum
   * */
  def double(rng: RNG): (Double, RNG) = {
    val (i, nextRng) = nonNegativeInt(rng)
    val result: Double = i / Int.MaxValue.toDouble + 1
    (result, nextRng)
  }

  /**
   * 6.5 Use map to reimplement double
   */
  def doubleMap(rng: RNG): Rand[(Double, RNG)] = {
    map(double){
      i => (i / (Int.MaxValue.toDouble + 1), rng)
    }
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, nextRNG) = double(r1)
    ((i, d), nextRNG)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), nextRNG) = intDouble(rng)
    ((d, i), nextRNG)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  /**
   * 6.4 Write a function to generate a list of random integers
   */
  def ints(count: Int)(rng: SimpleRNG): (List[Int], RNG) = {
    val emptyAccumulator = List()
    @annotation.tailrec
    def go(n: Int, resultsSoFar: List[Int], nextRNG: RNG): (List[Int], RNG) = {
      if (n <= 0) (resultsSoFar, nextRNG)
      else {
        val (i: Int, rng) = nextRNG.nextInt
        go(n-1, resultsSoFar=resultsSoFar :+ i, nextRNG=rng)
      }
    }
    go(count, emptyAccumulator, rng)
  }
}
