# Data Constructor
A data constructor declaration gives us a function to construct object of that data type

```scala
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head:A, tail:List[A]) extends List[A]
```

# Variance Annotation
`+` in front of type parameter `A` is a variance annotation that signals that `A` is a *covariant* parameter

`List[Dog]` is a subtype of `List[Animal]` if `Dog` is a substype of `Animal` 

This would not have been possible without variance annotation.
`List[Nothing]` is a subtype of `List[Int]` because `Nothing` is a subtype of `Int`
```sbtshell
scala> val x:List[Int] = Nil
x: List[Int] = Nil
```

# Companion Object
An *object* with the same name as the data type where we can put convenience functions of the data type

# Pattern Matching
A pattern matches if there exists an assignment of variables in the pattern to subexpressions of the target that make it *structurally equivalent* to the target

```scala
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head:A, tail:List[A]) extends List[A]

object List {
def sum(ints: List[Int]):Int = ints match {
  case Nil => 0
  case Cons(x, xs) => x + sum(xs)
}

def apply[A](as: A*): List[A] =
  if(as.isEmpty) Nil
  else Cons(as.head , apply(as.tail:_*))
}

val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
} 
//x: Int = 3
```

# Variadic Function
A function that accepts zero or more arguments of type `A`

The special `_*` type annotation allows us to pass a `Seq` to a variadic method
```scala
def apply[A](as: A*): List[A] =
  if(as.isEmpty) Nil
  else Cons(as.head , apply(as.tail:_*))
}
```

# Data Sharing
Immutable data can be reused without having to worry about subsequent code modifying our data. There is no need to make copies to avoid modification or corruption.

# Algebraic Data Type
An ADT is a data type defined by one or more data constructors, each of which may contain zero or more arguments. 
We say the data type is *sum* or *union* of its data constructors, and each data constructor is the *product* of its arguments