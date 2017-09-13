# Procedure
Some parameterized chunk of code that may have side effects

# Referential Transparency
An expression *e* is *referential transparent* if for all programs *p*, all occurrences of *e* in *p* can be replaced by the expression *e* itself without affecting the meaning of *p*

`r1` == `r2`
```scala
val x = new StringBuilder("Hello")
//x: StringBuilder = Hello

val y = x.append(", World")
//y: StringBuilder = Hello, World

val r1 = y.toString
//r1: String = Hello, World

val r2 = y.toString
//r2: String = Hello, World
```

Now we substitute all occurrences of y

`r1` != `r2`
```scala
val x = new StringBuilder("Hello")
//x: StringBuilder = Hello

val r1 = x.append(", World").toString
//r1: String = Hello, World

val r2 = x.append(", World").toString
//r2: String = Hello, World, World
```

