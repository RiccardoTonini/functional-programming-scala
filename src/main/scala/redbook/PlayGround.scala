package redbook

class PlayGround extends App {
    val ones: chapter5.Stream[Int] = chapter5.Stream.cons(1, ones)
    println("----------- map.exists  -----------")
    println(ones.map(_ + 1).exists(_ % 2 == 0))
    println("----------- takeWhile -----------")
    println(ones.takeWhile(_ == 1))
    println("----------- forAll -----------")
    println(ones.forAll(_ != 1))
}
