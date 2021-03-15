package redbook.chapter6

import munit.FunSuite

class RandomGeneratorTest extends FunSuite {
  test("nonNegativeInt with seed 0") {
    val expected = 0
    val testGenerator = SimpleRNG(0)

    val (obtained, _) = testGenerator.nextInt

    assertEquals(obtained, expected)
  }
  test("nonNegativeInt with seed Int.MinValue") {
    val expected = -1932951552
    val testGenerator = SimpleRNG(Int.MinValue)

    val (obtained, _) = testGenerator.nextInt

    assertEquals(obtained, expected)
  }
  test("nonNegativeInt with seed Int.MaxValue") {
    val expected = 1932566803
    val testGenerator = SimpleRNG(Int.MaxValue)

    val (obtained, _) = testGenerator.nextInt

    assertEquals(obtained, expected)
  }
  test("ints with seed 0") {
    val testGenerator = SimpleRNG(0)
    val expectedInts = List(0, 4232237, 178803790, 758674372, 1565954732)
    val (actualInts, _) = RandomGenerator.ints(5)(testGenerator)

    assertEquals(actualInts , expectedInts)
  }

}
