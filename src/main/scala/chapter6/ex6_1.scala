package chapter6

object ex6_1 {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (Int.MinValue, nextRNG) => (math.abs(Int.MinValue + 1), nextRNG)
      case (nextValue, nextRNG) => (math.abs(nextValue), nextRNG)
    }
  }

  nonNegativeInt(SimpleRNG(42))
}
