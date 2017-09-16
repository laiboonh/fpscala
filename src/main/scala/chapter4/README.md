# Exceptions break RT
RT expressions does not depend on context and may be reasoned about locally.
Non RT expressions are context dependent and requires more global reasoning.
```scala
def failingFn(i:Int):Int = {
  val y: Int = throw new Exception("fail")
  try {
    val x = 42 + 5
    x + y
  }
  catch {
    case e:Exception => 43
  }
}
```

```sbtshell
scala> failingFn(10)
java.lang.Exception: fail
  at .failingFn(<console>:12)
  ... 32 elided
```

```scala
def failingFn(i:Int):Int = {
  try {
    val x = 42 + 5
    x + ((throw new Exception("fail")):Int)
  }
  catch {
    case e:Exception => 43
  }
}
```
Different response
```sbtshell
scala> failingFn(10)
res1: Int = 43
```

# Partial function
`mean` is not defined for some inputs
```scala
def mean(xs:Seq[Double]): Double = 
  if(xs.isEmpty)
    throw new ArithmeticException("mean of an empty list")
  else xs.sum / xs.length  
```

# Total function
Takes each value of the input type to exactly one value of the output type
```scala
def mean(xs:Seq[Double]): Option[Double] = 
  if(xs.isEmpty) None
  else Some(xs.sum / xs.length)  
```

# Lift
Turn a function `A => B` into `Option[A] => Option[B]`
```scala
def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f
```