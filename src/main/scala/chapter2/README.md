# Singleton
A class that has only a single named instance. It's like creating a new instance of an anonymous class.

Singleton
```sbtshell
scala> object Foo
//defined object Foo

scala> :type Foo
//Foo.type
```

Case class
```sbtshell
scala> case class Foo(foo:String)
defined class Foo

scala> val foo = Foo("foo")
foo: Foo = Foo(foo)

scala> :type foo
Foo
```

# Module
An object whose primary purpose is giving its members a namespace

# Tail recursion
There is no additional work left to do after the recursive call returns

```scala
object MyModule {
  //recursive
  def factorial(n:Int):Int = {
    if(n==0) 1
    else n * factorial(n - 1)
  }
  
  //tail recursive
  def factorial1(n:Int):Int = {
    def go(n:Int, acc:Int):Int = {
      if(n==0) acc
      else go(n-1, acc*n) 
    }      
    go(n, 1)  
  }
}
```

# Function Literal
When we define a functional literal, what is actually being defined is an object with an `apply` method implementation
```sbtshell
scala> val lessThan = (a:Int,b:Int) => a < b
fn: (Int, Int) => Boolean = <function2>
```
is equivalent to
```scala
val lessThan = new Function2[Int,Int,Boolean] {
  def apply(a:Int, b:Int) = a < b
}
```
When we call a function
```sbtshell
scala> lessThan(2,3)
res12: Boolean = true
```
is equivalent to
```sbtshell
scala> lessThan.apply(2,3)
res13: Boolean = true
```

# Partial application
The function is being applied to some but not all of the arguments
```scala
def partial[A,B,C](a:A, f: (A,B) => C): B => C = {
  (b:B) => f(a,b)
}
```

# Close over
Within the body of this inner function, the outer *a* is still in scope. We sometimes says that the inner function *closes over* its environment, which includes *a*
```scala
def partial[A,B,C](a:A, f: (A,B) => C): B => C = {
  b => f(a,b)
}
```

# Currying
Converts a function *f* of two arguments into a function of one argument that partially applies *f*
```scala
def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
  a => b => f(a,b)
}
```
The reverse of *currying*
```scala
def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
  (a, b) => f(a)(b)
}
```

# Function Composition
Feeds the output of one function to the input of another function
```scala
def compose[A, B, C](f: B => C, g: A => B): A => C = {
  a => f(g(a))
} 
```
We can use `g andThen f` or `f compose g` instead
