# Making stateful APIs pure
Having the API compute the next state rather than actually mutate anything.

Example: Make the caller responsible for passing the computed next state to the rest of the program.
```scala
trait RNG {
  //Instead of nextInt: Int we output the next state as well
  def nextInt: (Int, RNG)
}
case class SimpleRNG(seed:Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
```
If we want 2 random numbers this won't do
```scala
val rng = SimpleRNG(42)
val (i1, _) = rng.nextInt
//i1: Int = 16159453
val (i2, _) = rng.nextInt
//i2: Int = 16159453
```
Caller need to manage the state
```scala
val rng = SimpleRNG(42)
val (i1, rng1) = rng.nextInt
//i1: Int = 16159453
//rng1: RNG = SimpleRNG(1059025964525)
val (i2, rng2) = rng1.nextInt
//i2: Int = -1281479697
//rng2: RNG = SimpleRNG(197491923327988)
```
