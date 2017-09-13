package chapter3

import chapter3.ex3_28.Tree

object ex3_1 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }
}


object ex3_2 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def tail[A](ints: List[A]): List[A] = ints match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

}

object ex3_3 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def tail[A](ints: List[A]): List[A] = ints match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }

    def setHead[A](ints: List[A], a: A): List[A] = ints match {
      case Nil => Cons(a, Nil)
      case Cons(_, xs) => Cons(a, xs)
    }


    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

}

object ex3_4 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def tail[A](ints: List[A]): List[A] = {
      drop(ints, 1)
    }

    def setHead[A](ints: List[A], a: A): List[A] = ints match {
      case Nil => Cons(a, Nil)
      case Cons(_, xs) => Cons(a, xs)
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    }


    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

}


object ex3_5 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def tail[A](ints: List[A]): List[A] = {
      drop(ints, 1)
    }

    def setHead[A](ints: List[A], a: A): List[A] = ints match {
      case Nil => Cons(a, Nil)
      case Cons(_, xs) => Cons(a, xs)
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) Cons(x, dropWhile(xs, f)) else dropWhile(xs, f)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

}

object ex3_6 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def tail[A](ints: List[A]): List[A] = {
      drop(ints, 1)
    }

    def setHead[A](ints: List[A], a: A): List[A] = ints match {
      case Nil => Cons(a, Nil)
      case Cons(_, xs) => Cons(a, xs)
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) Cons(x, dropWhile(xs, f)) else dropWhile(xs, f)
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

}

object ex3_7 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def tail[A](ints: List[A]): List[A] = {
      drop(ints, 1)
    }

    def setHead[A](ints: List[A], a: A): List[A] = ints match {
      case Nil => Cons(a, Nil)
      case Cons(_, xs) => Cons(a, xs)
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) Cons(x, dropWhile(xs, f)) else dropWhile(xs, f)
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def sum(ints: List[Int]): Int =
      foldRight(ints, 0)((x, y) => x + y)

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

}

object ex3_9 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def tail[A](ints: List[A]): List[A] = {
      drop(ints, 1)
    }

    def setHead[A](ints: List[A], a: A): List[A] = ints match {
      case Nil => Cons(a, Nil)
      case Cons(_, xs) => Cons(a, xs)
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) Cons(x, dropWhile(xs, f)) else dropWhile(xs, f)
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def sum(ints: List[Int]): Int =
      foldRight(ints, 0)((x, y) => x + y)

    def length[A](as: List[A]): Int =
      foldRight(as, 0)((_, acc) => acc + 1)

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

}


object ex3_10 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def tail[A](ints: List[A]): List[A] = {
      drop(ints, 1)
    }

    def setHead[A](ints: List[A], a: A): List[A] = ints match {
      case Nil => Cons(a, Nil)
      case Cons(_, xs) => Cons(a, xs)
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) Cons(x, dropWhile(xs, f)) else dropWhile(xs, f)
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    def sum(ints: List[Int]): Int =
      foldRight(ints, 0)((x, y) => x + y)

    def length[A](as: List[A]): Int =
      foldRight(as, 0)((_, acc) => acc + 1)

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

}

object ex3_11 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def tail[A](ints: List[A]): List[A] = {
      drop(ints, 1)
    }

    def setHead[A](ints: List[A], a: A): List[A] = ints match {
      case Nil => Cons(a, Nil)
      case Cons(_, xs) => Cons(a, xs)
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) Cons(x, dropWhile(xs, f)) else dropWhile(xs, f)
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    def sum(ints: List[Int]): Int =
      foldLeft(ints, 0)((acc, x) => acc + x)

    def product(ints: List[Int]): Int =
      foldLeft(ints, 1)((acc, x) => acc * x)

    def length[A](as: List[A]): Int =
      foldLeft(as, 0)((acc, _) => acc + 1)

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

}

object ex3_12 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def tail[A](ints: List[A]): List[A] = {
      drop(ints, 1)
    }

    def setHead[A](ints: List[A], a: A): List[A] = ints match {
      case Nil => Cons(a, Nil)
      case Cons(_, xs) => Cons(a, xs)
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) Cons(x, dropWhile(xs, f)) else dropWhile(xs, f)
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    def sum(ints: List[Int]): Int =
      foldLeft(ints, 0)((acc, x) => acc + x)

    def product(ints: List[Int]): Int =
      foldLeft(ints, 1)((acc, x) => acc * x)

    def length[A](as: List[A]): Int =
      foldLeft(as, 0)((acc, _) => acc + 1)

    def reverse[A](as: List[A]): List[A] =
      foldLeft(as, Nil: List[A])((acc, x) => Cons(x, acc))

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

}

object ex3_13 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def tail[A](ints: List[A]): List[A] = {
      drop(ints, 1)
    }

    def setHead[A](ints: List[A], a: A): List[A] = ints match {
      case Nil => Cons(a, Nil)
      case Cons(_, xs) => Cons(a, xs)
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) Cons(x, dropWhile(xs, f)) else dropWhile(xs, f)
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(reverse(xs), z)((b, a) => f(a, b))
    }

    def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldRight(xs, (b: B) => b)((a, g) => b => g(f(b, a)))(z)
    }

    def sum(ints: List[Int]): Int =
      foldLeft(ints, 0)((acc, x) => acc + x)

    def product(ints: List[Int]): Int =
      foldLeft(ints, 1)((acc, x) => acc * x)

    def length[A](as: List[A]): Int =
      foldLeft(as, 0)((acc, _) => acc + 1)

    def reverse[A](as: List[A]): List[A] =
      foldLeft(as, Nil: List[A])((acc, x) => Cons(x, acc))

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

}

object ex3_14 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def tail[A](ints: List[A]): List[A] = {
      drop(ints, 1)
    }

    def setHead[A](ints: List[A], a: A): List[A] = ints match {
      case Nil => Cons(a, Nil)
      case Cons(_, xs) => Cons(a, xs)
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) Cons(x, dropWhile(xs, f)) else dropWhile(xs, f)
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(reverse(xs), z)((b, a) => f(a, b))
    }

    def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldRight(xs, (b: B) => b)((a, g) => b => g(f(b, a)))(z)
    }

    def sum(ints: List[Int]): Int =
      foldLeft(ints, 0)((acc, x) => acc + x)

    def product(ints: List[Int]): Int =
      foldLeft(ints, 1)((acc, x) => acc * x)

    def length[A](as: List[A]): Int =
      foldLeft(as, 0)((acc, _) => acc + 1)

    def reverse[A](as: List[A]): List[A] =
      foldLeft(as, Nil: List[A])((acc, x) => Cons(x, acc))

    def append[A](l: List[A], r: List[A]) =
      foldRight(l, r)((x, acc) => Cons(x, acc))

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

}

object ex3_15 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def tail[A](ints: List[A]): List[A] = {
      drop(ints, 1)
    }

    def setHead[A](ints: List[A], a: A): List[A] = ints match {
      case Nil => Cons(a, Nil)
      case Cons(_, xs) => Cons(a, xs)
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) Cons(x, dropWhile(xs, f)) else dropWhile(xs, f)
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(reverse(xs), z)((b, a) => f(a, b))
    }

    def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldRight(xs, (b: B) => b)((a, g) => b => g(f(b, a)))(z)
    }

    def flatten[A](l: List[List[A]]) = {
      //      def go(lst:List[List[A]], acc:List[A]):List[A] = lst match {
      //        case Nil => acc
      //        case Cons(x,xs) => append(x, go(xs, acc))
      //      }
      //      go(l, Nil:List[A])
      foldRight(l, Nil: List[A])(append)
    }

    def sum(ints: List[Int]): Int =
      foldLeft(ints, 0)((acc, x) => acc + x)

    def product(ints: List[Int]): Int =
      foldLeft(ints, 1)((acc, x) => acc * x)

    def length[A](as: List[A]): Int =
      foldLeft(as, 0)((acc, _) => acc + 1)

    def reverse[A](as: List[A]): List[A] =
      foldLeft(as, Nil: List[A])((acc, x) => Cons(x, acc))

    def append[A](l: List[A], r: List[A]) =
      foldRight(l, r)((x, acc) => Cons(x, acc))

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

}

object ex3_16 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def tail[A](ints: List[A]): List[A] = {
      drop(ints, 1)
    }

    def setHead[A](ints: List[A], a: A): List[A] = ints match {
      case Nil => Cons(a, Nil)
      case Cons(_, xs) => Cons(a, xs)
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) Cons(x, dropWhile(xs, f)) else dropWhile(xs, f)
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(reverse(xs), z)((b, a) => f(a, b))
    }

    def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldRight(xs, (b: B) => b)((a, g) => b => g(f(b, a)))(z)
    }

    def flatten[A](l: List[List[A]]) = {
      //      def go(lst:List[List[A]], acc:List[A]):List[A] = lst match {
      //        case Nil => acc
      //        case Cons(x,xs) => append(x, go(xs, acc))
      //      }
      //      go(l, Nil:List[A])
      foldRight(l, Nil: List[A])(append)
    }

    def addOne(l: List[Int]): List[Int] =
      foldRight(l, Nil: List[Int])((x, acc) => Cons((x + 1), acc))

    def sum(ints: List[Int]): Int =
      foldLeft(ints, 0)((acc, x) => acc + x)

    def product(ints: List[Int]): Int =
      foldLeft(ints, 1)((acc, x) => acc * x)

    def length[A](as: List[A]): Int =
      foldLeft(as, 0)((acc, _) => acc + 1)

    def reverse[A](as: List[A]): List[A] =
      foldLeft(as, Nil: List[A])((acc, x) => Cons(x, acc))

    def append[A](l: List[A], r: List[A]) =
      foldRight(l, r)((x, acc) => Cons(x, acc))

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

}

object ex3_17 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def tail[A](ints: List[A]): List[A] = {
      drop(ints, 1)
    }

    def setHead[A](ints: List[A], a: A): List[A] = ints match {
      case Nil => Cons(a, Nil)
      case Cons(_, xs) => Cons(a, xs)
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) Cons(x, dropWhile(xs, f)) else dropWhile(xs, f)
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(reverse(xs), z)((b, a) => f(a, b))
    }

    def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldRight(xs, (b: B) => b)((a, g) => b => g(f(b, a)))(z)
    }

    def flatten[A](l: List[List[A]]) = {
      //      def go(lst:List[List[A]], acc:List[A]):List[A] = lst match {
      //        case Nil => acc
      //        case Cons(x,xs) => append(x, go(xs, acc))
      //      }
      //      go(l, Nil:List[A])
      foldRight(l, Nil: List[A])(append)
    }

    def addOne(l: List[Int]): List[Int] =
      foldRight(l, Nil: List[Int])((x, acc) => Cons((x + 1), acc))

    def dblString(l: List[Double]): List[String] =
      foldRight(l, Nil: List[String])((x, acc) => Cons((x.toString), acc))

    def sum(ints: List[Int]): Int =
      foldLeft(ints, 0)((acc, x) => acc + x)

    def product(ints: List[Int]): Int =
      foldLeft(ints, 1)((acc, x) => acc * x)

    def length[A](as: List[A]): Int =
      foldLeft(as, 0)((acc, _) => acc + 1)

    def reverse[A](as: List[A]): List[A] =
      foldLeft(as, Nil: List[A])((acc, x) => Cons(x, acc))

    def append[A](l: List[A], r: List[A]) =
      foldRight(l, r)((x, acc) => Cons(x, acc))

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

}

object ex3_18 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def tail[A](ints: List[A]): List[A] = {
      drop(ints, 1)
    }

    def setHead[A](ints: List[A], a: A): List[A] = ints match {
      case Nil => Cons(a, Nil)
      case Cons(_, xs) => Cons(a, xs)
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) Cons(x, dropWhile(xs, f)) else dropWhile(xs, f)
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(reverse(xs), z)((b, a) => f(a, b))
    }

    def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldRight(xs, (b: B) => b)((a, g) => b => g(f(b, a)))(z)
    }

    def flatten[A](l: List[List[A]]) =
      foldRight(l, Nil: List[A])(append)

    def addOne(l: List[Int]): List[Int] =
      foldRight(l, Nil: List[Int])((x, acc) => Cons((x + 1), acc))

    def dblString(l: List[Double]): List[String] =
      foldRight(l, Nil: List[String])((x, acc) => Cons((x.toString), acc))

    def map[A, B](as: List[A])(f: A => B): List[B] =
    //      foldRight(as, Nil:List[B])((x,acc)=> Cons(f(x), acc))
      foldRightViaFoldLeft(as, Nil: List[B])((x, acc) => Cons(f(x), acc))

    def sum(ints: List[Int]): Int =
      foldLeft(ints, 0)((acc, x) => acc + x)

    def product(ints: List[Int]): Int =
      foldLeft(ints, 1)((acc, x) => acc * x)

    def length[A](as: List[A]): Int =
      foldLeft(as, 0)((acc, _) => acc + 1)

    def reverse[A](as: List[A]): List[A] =
      foldLeft(as, Nil: List[A])((acc, x) => Cons(x, acc))

    def append[A](l: List[A], r: List[A]) =
      foldRight(l, r)((x, acc) => Cons(x, acc))

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

}

object ex3_19 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def tail[A](ints: List[A]): List[A] = {
      drop(ints, 1)
    }

    def setHead[A](ints: List[A], a: A): List[A] = ints match {
      case Nil => Cons(a, Nil)
      case Cons(_, xs) => Cons(a, xs)
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) Cons(x, dropWhile(xs, f)) else dropWhile(xs, f)
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(reverse(xs), z)((b, a) => f(a, b))
    }

    def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldRight(xs, (b: B) => b)((a, g) => b => g(f(b, a)))(z)
    }

    def flatten[A](l: List[List[A]]) =
      foldRight(l, Nil: List[A])(append)

    def addOne(l: List[Int]): List[Int] =
      foldRight(l, Nil: List[Int])((x, acc) => Cons((x + 1), acc))

    def dblString(l: List[Double]): List[String] =
      foldRight(l, Nil: List[String])((x, acc) => Cons((x.toString), acc))

    def map[A, B](as: List[A])(f: A => B): List[B] =
    //      foldRight(as, Nil:List[B])((x,acc)=> Cons(f(x), acc))
      foldRightViaFoldLeft(as, Nil: List[B])((x, acc) => Cons(f(x), acc))

    def sum(ints: List[Int]): Int =
      foldLeft(ints, 0)((acc, x) => acc + x)

    def product(ints: List[Int]): Int =
      foldLeft(ints, 1)((acc, x) => acc * x)

    def length[A](as: List[A]): Int =
      foldLeft(as, 0)((acc, _) => acc + 1)

    def reverse[A](as: List[A]): List[A] =
      foldLeft(as, Nil: List[A])((acc, x) => Cons(x, acc))

    def append[A](l: List[A], r: List[A]) =
      foldRight(l, r)((x, acc) => Cons(x, acc))

    def filter[A](as: List[A])(f: A => Boolean): List[A] =
      foldRight(as, Nil: List[A])((x, acc) => if (f(x)) Cons(x, acc) else acc)

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

}

object ex3_20 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def tail[A](ints: List[A]): List[A] = {
      drop(ints, 1)
    }

    def setHead[A](ints: List[A], a: A): List[A] = ints match {
      case Nil => Cons(a, Nil)
      case Cons(_, xs) => Cons(a, xs)
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) Cons(x, dropWhile(xs, f)) else dropWhile(xs, f)
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(reverse(xs), z)((b, a) => f(a, b))
    }

    def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldRight(xs, (b: B) => b)((a, g) => b => g(f(b, a)))(z)
    }

    def flatten[A](l: List[List[A]]) =
      foldRight(l, Nil: List[A])(append)

    def addOne(l: List[Int]): List[Int] =
      foldRight(l, Nil: List[Int])((x, acc) => Cons((x + 1), acc))

    def dblString(l: List[Double]): List[String] =
      foldRight(l, Nil: List[String])((x, acc) => Cons((x.toString), acc))

    def map[A, B](as: List[A])(f: A => B): List[B] =
      foldRightViaFoldLeft(as, Nil: List[B])((x, acc) => Cons(f(x), acc))

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
      foldRightViaFoldLeft(as, Nil: List[B])((x, acc) => append(f(x), acc))

    def sum(ints: List[Int]): Int =
      foldLeft(ints, 0)((acc, x) => acc + x)

    def product(ints: List[Int]): Int =
      foldLeft(ints, 1)((acc, x) => acc * x)

    def length[A](as: List[A]): Int =
      foldLeft(as, 0)((acc, _) => acc + 1)

    def reverse[A](as: List[A]): List[A] =
      foldLeft(as, Nil: List[A])((acc, x) => Cons(x, acc))

    def append[A](l: List[A], r: List[A]) =
      foldRight(l, r)((x, acc) => Cons(x, acc))

    def filter[A](as: List[A])(f: A => Boolean): List[A] =
      foldRight(as, Nil: List[A])((x, acc) => if (f(x)) Cons(x, acc) else acc)

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

}

object ex3_21 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def tail[A](ints: List[A]): List[A] = {
      drop(ints, 1)
    }

    def setHead[A](ints: List[A], a: A): List[A] = ints match {
      case Nil => Cons(a, Nil)
      case Cons(_, xs) => Cons(a, xs)
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) Cons(x, dropWhile(xs, f)) else dropWhile(xs, f)
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(reverse(xs), z)((b, a) => f(a, b))
    }

    def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldRight(xs, (b: B) => b)((a, g) => b => g(f(b, a)))(z)
    }

    def flatten[A](l: List[List[A]]) =
      foldRight(l, Nil: List[A])(append)

    def addOne(l: List[Int]): List[Int] =
      foldRight(l, Nil: List[Int])((x, acc) => Cons((x + 1), acc))

    def dblString(l: List[Double]): List[String] =
      foldRight(l, Nil: List[String])((x, acc) => Cons((x.toString), acc))

    def map[A, B](as: List[A])(f: A => B): List[B] =
      foldRightViaFoldLeft(as, Nil: List[B])((x, acc) => Cons(f(x), acc))

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
      foldRightViaFoldLeft(as, Nil: List[B])((x, acc) => append(f(x), acc))

    def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
      flatMap(as)((a: A) => if (f(a)) Cons(a, Nil) else Nil)

    def sum(ints: List[Int]): Int =
      foldLeft(ints, 0)((acc, x) => acc + x)

    def product(ints: List[Int]): Int =
      foldLeft(ints, 1)((acc, x) => acc * x)

    def length[A](as: List[A]): Int =
      foldLeft(as, 0)((acc, _) => acc + 1)

    def reverse[A](as: List[A]): List[A] =
      foldLeft(as, Nil: List[A])((acc, x) => Cons(x, acc))

    def append[A](l: List[A], r: List[A]) =
      foldRight(l, r)((x, acc) => Cons(x, acc))

    def filter[A](as: List[A])(f: A => Boolean): List[A] =
      foldRight(as, Nil: List[A])((x, acc) => if (f(x)) Cons(x, acc) else acc)

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

}

object ex3_22 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def tail[A](ints: List[A]): List[A] = {
      drop(ints, 1)
    }

    def setHead[A](ints: List[A], a: A): List[A] = ints match {
      case Nil => Cons(a, Nil)
      case Cons(_, xs) => Cons(a, xs)
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) Cons(x, dropWhile(xs, f)) else dropWhile(xs, f)
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(reverse(xs), z)((b, a) => f(a, b))
    }

    def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldRight(xs, (b: B) => b)((a, g) => b => g(f(b, a)))(z)
    }

    def flatten[A](l: List[List[A]]) =
      foldRight(l, Nil: List[A])(append)

    def addOne(l: List[Int]): List[Int] =
      foldRight(l, Nil: List[Int])((x, acc) => Cons((x + 1), acc))

    def dblString(l: List[Double]): List[String] =
      foldRight(l, Nil: List[String])((x, acc) => Cons((x.toString), acc))

    def map[A, B](as: List[A])(f: A => B): List[B] =
      foldRightViaFoldLeft(as, Nil: List[B])((x, acc) => Cons(f(x), acc))

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
      foldRightViaFoldLeft(as, Nil: List[B])((x, acc) => append(f(x), acc))

    def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
      flatMap(as)((a: A) => if (f(a)) Cons(a, Nil) else Nil)

    def sumList(as: List[Int], bs: List[Int]): List[Int] = {
      def go(as: List[Int], bs: List[Int], acc: List[Int]): List[Int] = as match {
        case Nil => acc
        case Cons(x, xs) => bs match {
          case Nil => Nil
          case Cons(y, ys) => go(xs, ys, Cons((x + y), acc))
        }

      }

      go(as, bs, Nil: List[Int])
    }

    def sum(ints: List[Int]): Int =
      foldLeft(ints, 0)((acc, x) => acc + x)

    def product(ints: List[Int]): Int =
      foldLeft(ints, 1)((acc, x) => acc * x)

    def length[A](as: List[A]): Int =
      foldLeft(as, 0)((acc, _) => acc + 1)

    def reverse[A](as: List[A]): List[A] =
      foldLeft(as, Nil: List[A])((acc, x) => Cons(x, acc))

    def append[A](l: List[A], r: List[A]) =
      foldRight(l, r)((x, acc) => Cons(x, acc))

    def filter[A](as: List[A])(f: A => Boolean): List[A] =
      foldRight(as, Nil: List[A])((x, acc) => if (f(x)) Cons(x, acc) else acc)

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

}

object ex3_23 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def tail[A](ints: List[A]): List[A] = {
      drop(ints, 1)
    }

    def setHead[A](ints: List[A], a: A): List[A] = ints match {
      case Nil => Cons(a, Nil)
      case Cons(_, xs) => Cons(a, xs)
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) Cons(x, dropWhile(xs, f)) else dropWhile(xs, f)
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(reverse(xs), z)((b, a) => f(a, b))
    }

    def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldRight(xs, (b: B) => b)((a, g) => b => g(f(b, a)))(z)
    }

    def flatten[A](l: List[List[A]]) =
      foldRight(l, Nil: List[A])(append)

    def addOne(l: List[Int]): List[Int] =
      foldRight(l, Nil: List[Int])((x, acc) => Cons((x + 1), acc))

    def dblString(l: List[Double]): List[String] =
      foldRight(l, Nil: List[String])((x, acc) => Cons((x.toString), acc))

    def map[A, B](as: List[A])(f: A => B): List[B] =
      foldRightViaFoldLeft(as, Nil: List[B])((x, acc) => Cons(f(x), acc))

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
      foldRightViaFoldLeft(as, Nil: List[B])((x, acc) => append(f(x), acc))

    def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
      flatMap(as)((a: A) => if (f(a)) Cons(a, Nil) else Nil)

    def sumList(as: List[Int], bs: List[Int]): List[Int] = {
      def go(as: List[Int], bs: List[Int], acc: List[Int]): List[Int] = as match {
        case Nil => acc
        case Cons(x, xs) => bs match {
          case Nil => Nil
          case Cons(y, ys) => go(xs, ys, Cons((x + y), acc))
        }

      }

      go(as, bs, Nil: List[Int])
    }

    def zipWith[A](as: List[A], bs: List[A])(f: (A, A) => A): List[A] = {
      def go(as: List[A], bs: List[A], acc: List[A]): List[A] = as match {
        case Nil => acc
        case Cons(x, xs) => bs match {
          case Nil => Nil
          case Cons(y, ys) => go(xs, ys, Cons(f(x, y), acc))
        }
      }

      go(as, bs, Nil: List[A])
    }

    def sum(ints: List[Int]): Int =
      foldLeft(ints, 0)((acc, x) => acc + x)

    def product(ints: List[Int]): Int =
      foldLeft(ints, 1)((acc, x) => acc * x)

    def length[A](as: List[A]): Int =
      foldLeft(as, 0)((acc, _) => acc + 1)

    def reverse[A](as: List[A]): List[A] =
      foldLeft(as, Nil: List[A])((acc, x) => Cons(x, acc))

    def append[A](l: List[A], r: List[A]) =
      foldRight(l, r)((x, acc) => Cons(x, acc))

    def filter[A](as: List[A])(f: A => Boolean): List[A] =
      foldRight(as, Nil: List[A])((x, acc) => if (f(x)) Cons(x, acc) else acc)

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

}

object ex3_24 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def tail[A](ints: List[A]): List[A] = {
      drop(ints, 1)
    }

    def setHead[A](ints: List[A], a: A): List[A] = ints match {
      case Nil => Cons(a, Nil)
      case Cons(_, xs) => Cons(a, xs)
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) Cons(x, dropWhile(xs, f)) else dropWhile(xs, f)
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(reverse(xs), z)((b, a) => f(a, b))
    }

    def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldRight(xs, (b: B) => b)((a, g) => b => g(f(b, a)))(z)
    }

    def flatten[A](l: List[List[A]]) =
      foldRight(l, Nil: List[A])(append)

    def addOne(l: List[Int]): List[Int] =
      foldRight(l, Nil: List[Int])((x, acc) => Cons((x + 1), acc))

    def dblString(l: List[Double]): List[String] =
      foldRight(l, Nil: List[String])((x, acc) => Cons((x.toString), acc))

    def map[A, B](as: List[A])(f: A => B): List[B] =
      foldRightViaFoldLeft(as, Nil: List[B])((x, acc) => Cons(f(x), acc))

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
      foldRightViaFoldLeft(as, Nil: List[B])((x, acc) => append(f(x), acc))

    def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
      flatMap(as)((a: A) => if (f(a)) Cons(a, Nil) else Nil)

    def sumList(as: List[Int], bs: List[Int]): List[Int] = {
      def go(as: List[Int], bs: List[Int], acc: List[Int]): List[Int] = as match {
        case Nil => acc
        case Cons(x, xs) => bs match {
          case Nil => Nil
          case Cons(y, ys) => go(xs, ys, Cons((x + y), acc))
        }

      }

      go(as, bs, Nil: List[Int])
    }

    def zipWith[A](as: List[A], bs: List[A])(f: (A, A) => A): List[A] = {
      def go(as: List[A], bs: List[A], acc: List[A]): List[A] = as match {
        case Nil => acc
        case Cons(x, xs) => bs match {
          case Nil => Nil
          case Cons(y, ys) => go(xs, ys, Cons(f(x, y), acc))
        }
      }

      go(as, bs, Nil: List[A])
    }

    def startsWith[A](as: List[A], prefix: List[A]): Boolean = (as, prefix) match {
      case (_, Nil) => true
      case (Cons(x, xs), Cons(y, ys)) => if (x == y) startsWith(xs, ys) else false
      case (Nil, _) => false
    }

    def hasSubsequences[A](sup: List[A], sub: List[A]): Boolean = sup match {
      case Nil => false
      case xs@Cons(a, as) => if (startsWith(xs, sub)) true else hasSubsequences(as, sub)
    }

    def sum(ints: List[Int]): Int =
      foldLeft(ints, 0)((acc, x) => acc + x)

    def product(ints: List[Int]): Int =
      foldLeft(ints, 1)((acc, x) => acc * x)

    def length[A](as: List[A]): Int =
      foldLeft(as, 0)((acc, _) => acc + 1)

    def reverse[A](as: List[A]): List[A] =
      foldLeft(as, Nil: List[A])((acc, x) => Cons(x, acc))

    def append[A](l: List[A], r: List[A]) =
      foldRight(l, r)((x, acc) => Cons(x, acc))

    def filter[A](as: List[A])(f: A => Boolean): List[A] =
      foldRight(as, Nil: List[A])((x, acc) => if (f(x)) Cons(x, acc) else acc)

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

}

object ex3_25 {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r)
  }
}

object ex3_26 {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(l, r) => maximum(l) max maximum(r)
  }
}

object ex3_27 {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](tree: Tree[A]): Int = {
    def go(t: Tree[A], acc: Int): Int = t match {
      case Leaf(_) => acc
      case Branch(l, r) => go(l, acc + 1) max go(r, acc + 1)
    }

    go(tree, 0)
  }

  //Branch(Branch(Leaf("ll"), Leaf("lr")), Leaf("r"))
}

object ex3_28 {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](tree: Tree[A]): Int = {
    def go(t: Tree[A], acc: Int): Int = t match {
      case Leaf(_) => acc
      case Branch(l, r) => go(l, acc + 1) max go(r, acc + 1)
    }

    go(tree, 0)
  }

  def depth1[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(l, r) => depth1(l) + 1 max depth1(r) + 1
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  //Branch(Branch(Leaf("ll"), Leaf("lr")), Leaf("r"))
}

object ex3_29 {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def fold[A, B](tree: Tree[A])(id: A => B)(f: (Tree[A], Tree[A]) => B): B = tree match {
    case Leaf(value) => id(value)
    case Branch(l, r) => f(l, r)
  }

  def size[A](tree: Tree[A]): Int = fold(tree)(_ => 1)((l, r) => size(l) + size(r))

  def maximum(tree: Tree[Int]): Int = fold(tree)(a => a)((l, r) => maximum(l) max maximum(r))

  def depth[A](tree: Tree[A]): Int = fold(tree)(_ => 0)((l, r)=> depth(l)+1 max depth(r)+1)

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(value => Leaf(f(value)):Tree[B])((l, r) => Branch(map(l)(f), map(r)(f)))
}