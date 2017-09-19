# Non-strict
A non-strict function may choose *not* to evaluate one or more of its arguments

# Strict
A strict function always evaluates its arguments
For example you will get an exception before `square` has a chance to do anything
```scala
def square(x:Int):Int = x * x
square(sys.error("failure")) 
```

# Thunk
The unevaluated form of an expression is called a thunk. We *force a thunk* to evaluate the expression and get a result.

# Cache
An argument that's passed unevaluated to a function will be evaluated once for each place its referenced in the function
```sbtshell
def maybeTwice(b:Boolean, i: =>Int) = if(b) i+i else 0
val x = maybeTwice(true, {
  println("hi")
  1+41
})
//hi
//hi
//x: Int = 84
```
We can cache the result once using `lazy` keyword
```sbtshell
def maybeTwice(b:Boolean, i: =>Int) = {
  lazy val j = i
  if(b) j+j else 0
}
val x = maybeTwice(true, {
  println("hi")
  1+41
})
//hi
//x: Int = 84
``` 
# Formal definition of strictness
An expression that *evaluates to bottom* - An expression that doesn't terminate

A function `f` is *strict* if the expression `f(x)` evaluates to bottom for all `x` that evaluate to bottom.

# Call by name
A non strict function takes its arguments by *name*

# Call by value
A strict function takes it arguments by *value*

# Smart Constructors
A function for constructing a data type that ensures some additional invariant or provides a slightly different signature than the *real* constructors.
By convention smart constructors typically lowercase the 1st letter of the corresponding *real* constructor

# Separation of concerns
Separate the description of computation from actually running them

# Corecursive function (guarded recursion)
A recursive function consumes data, whereas a corecursive function produces data.
Recursive functions terminate by recursing on smaller inputs, corecursive functions need not terminate so long as they remain productive(doesn't *coterminate*).