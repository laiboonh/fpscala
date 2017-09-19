package chapter5

object ex5_1 {

  import Stream._

  sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Cons(hd, tl) => cons(hd(), tl()).toList
      case Empty => List[A]()
    }
  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = {
        println("saving head")
        hd
      }
      lazy val tail = {
        println("saving tail")
        tl
      }
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))
  }

}

object ex5_2 {

  import Stream._

  sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Cons(hd, tl) => hd() :: tl().toList
      case Empty => List[A]()
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(hd, tl) if n > 1 => cons(hd(), tl().take(n - 1))
      case Cons(hd, _) if n == 1 => cons(hd(), empty)
      case Empty => empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, tl) if n > 0 => tl().drop(n - 1)
      case _ => this
    }
  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = {
        println("saving head")
        hd
      }
      lazy val tail = {
        println("saving tail")
        tl
      }
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))
  }

}

object ex5_3 {

  import Stream._

  sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Cons(hd, tl) => hd() :: tl().toList
      case Empty => List[A]()
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(hd, tl) if n > 1 => cons(hd(), tl().take(n - 1))
      case Cons(hd, _) if n == 1 => cons(hd(), empty)
      case Empty => empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, tl) if n > 0 => tl().drop(n - 1)
      case _ => this
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(hd, tl) if p(hd()) => cons(hd(), tl().takeWhile(p))
      case _ => empty
    }
  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = {
        println("saving head")
        hd
      }
      lazy val tail = {
        println("saving tail")
        tl
      }
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))
  }

}

object ex5_4 {

  import Stream._

  sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Cons(hd, tl) => hd() :: tl().toList
      case Empty => List[A]()
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(hd, tl) if n > 1 => cons(hd(), tl().take(n - 1))
      case Cons(hd, _) if n == 1 => cons(hd(), empty)
      case Empty => empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, tl) if n > 0 => tl().drop(n - 1)
      case _ => this
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(hd, tl) if p(hd()) => cons(hd(), tl().takeWhile(p))
      case _ => empty
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = {
        println("saving head")
        hd
      }
      lazy val tail = {
        println("saving tail")
        tl
      }
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))
  }

}

object ex5_5 {

  import Stream._

  sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Cons(hd, tl) => hd() :: tl().toList
      case Empty => List[A]()
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(hd, tl) if n > 1 => cons(hd(), tl().take(n - 1))
      case Cons(hd, _) if n == 1 => cons(hd(), empty)
      case Empty => empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, tl) if n > 0 => tl().drop(n - 1)
      case _ => this
    }

    //    def takeWhile(p: A => Boolean): Stream[A] = this match {
    //      case Cons(hd, tl) if p(hd()) => cons(hd(), tl().takeWhile(p))
    //      case _ => empty
    //    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    def takeWhile(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) => if (!p(a)) empty else cons(a, b))


  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = {
        println("saving head")
        hd
      }
      lazy val tail = {
        println("saving tail")
        tl
      }
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))
  }

}

object ex5_6 {

  import Stream._

  sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Cons(hd, tl) => hd() :: tl().toList
      case Empty => List[A]()
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(hd, tl) if n > 1 => cons(hd(), tl().take(n - 1))
      case Cons(hd, _) if n == 1 => cons(hd(), empty)
      case Empty => empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, tl) if n > 0 => tl().drop(n - 1)
      case _ => this
    }

    //    def takeWhile(p: A => Boolean): Stream[A] = this match {
    //      case Cons(hd, tl) if p(hd()) => cons(hd(), tl().takeWhile(p))
    //      case _ => empty
    //    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    def takeWhile(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) => if (!p(a)) empty else cons(a, b))

    def headOption: Option[A] =
      foldRight(None: Option[A])((a, _) => Some(a))


  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = {
        println("saving head")
        hd
      }
      lazy val tail = {
        println("saving tail")
        tl
      }
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))
  }

}

object ex5_7 {

  import Stream._

  sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Cons(hd, tl) => hd() :: tl().toList
      case Empty => List[A]()
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(hd, tl) if n > 1 => cons(hd(), tl().take(n - 1))
      case Cons(hd, _) if n == 1 => cons(hd(), empty)
      case Empty => empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, tl) if n > 0 => tl().drop(n - 1)
      case _ => this
    }

    //    def takeWhile(p: A => Boolean): Stream[A] = this match {
    //      case Cons(hd, tl) if p(hd()) => cons(hd(), tl().takeWhile(p))
    //      case _ => empty
    //    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    def takeWhile(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) => if (!p(a)) empty else cons(a, b))

    def headOption: Option[A] =
      foldRight(None: Option[A])((a, _) => Some(a))

    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B])((a, b) => cons(f(a), b))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B])((a, b) => b.append(f(a)))

    def filter(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

    def append[AA >: A](that: => Stream[AA]): Stream[AA] =
      foldRight(that)((a, b) => cons(a, b))

  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = {
        println("saving head")
        hd
      }
      lazy val tail = {
        println("saving tail")
        tl
      }
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))
  }

}

object ex5_8 {

  import Stream._

  sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Cons(hd, tl) => hd() :: tl().toList
      case Empty => List[A]()
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(hd, tl) if n > 1 => cons(hd(), tl().take(n - 1))
      case Cons(hd, _) if n == 1 => cons(hd(), empty)
      case Empty => empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, tl) if n > 0 => tl().drop(n - 1)
      case _ => this
    }

    //    def takeWhile(p: A => Boolean): Stream[A] = this match {
    //      case Cons(hd, tl) if p(hd()) => cons(hd(), tl().takeWhile(p))
    //      case _ => empty
    //    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    def takeWhile(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) => if (!p(a)) empty else cons(a, b))

    def headOption: Option[A] =
      foldRight(None: Option[A])((a, _) => Some(a))

    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B])((a, b) => cons(f(a), b))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B])((a, b) => b.append(f(a)))

    def filter(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

    def append[AA >: A](that: => Stream[AA]): Stream[AA] =
      foldRight(that)((a, b) => cons(a, b))

  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = {
        println("saving head")
        hd
      }
      lazy val tail = {
        println("saving tail")
        tl
      }
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

    def constant[A](a: A): Stream[A] = cons(a, constant(a))

  }

}

object ex5_9 {

  import Stream._

  sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Cons(hd, tl) => hd() :: tl().toList
      case Empty => List[A]()
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(hd, tl) if n > 1 => cons(hd(), tl().take(n - 1))
      case Cons(hd, _) if n == 1 => cons(hd(), empty)
      case Empty => empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, tl) if n > 0 => tl().drop(n - 1)
      case _ => this
    }

    //    def takeWhile(p: A => Boolean): Stream[A] = this match {
    //      case Cons(hd, tl) if p(hd()) => cons(hd(), tl().takeWhile(p))
    //      case _ => empty
    //    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    def takeWhile(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) => if (!p(a)) empty else cons(a, b))

    def headOption: Option[A] =
      foldRight(None: Option[A])((a, _) => Some(a))

    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B])((a, b) => cons(f(a), b))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B])((a, b) => b.append(f(a)))

    def filter(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

    def append[AA >: A](that: => Stream[AA]): Stream[AA] =
      foldRight(that)((a, b) => cons(a, b))

  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = {
        println("saving head")
        hd
      }
      lazy val tail = {
        println("saving tail")
        tl
      }
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  }

}

object ex5_10 {

  import Stream._

  sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Cons(hd, tl) => hd() :: tl().toList
      case Empty => List[A]()
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(hd, tl) if n > 1 => cons(hd(), tl().take(n - 1))
      case Cons(hd, _) if n == 1 => cons(hd(), empty)
      case Empty => empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, tl) if n > 0 => tl().drop(n - 1)
      case _ => this
    }

    //    def takeWhile(p: A => Boolean): Stream[A] = this match {
    //      case Cons(hd, tl) if p(hd()) => cons(hd(), tl().takeWhile(p))
    //      case _ => empty
    //    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    def takeWhile(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) => if (!p(a)) empty else cons(a, b))

    def headOption: Option[A] =
      foldRight(None: Option[A])((a, _) => Some(a))

    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B])((a, b) => cons(f(a), b))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B])((a, b) => b.append(f(a)))

    def filter(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

    def append[AA >: A](that: => Stream[AA]): Stream[AA] =
      foldRight(that)((a, b) => cons(a, b))

  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = {
        println("saving head")
        hd
      }
      lazy val tail = {
        println("saving tail")
        tl
      }
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    //0,1,1,2,3,5,8
    def fibs: Stream[Int] = {
      def go(pprev: Int, prev: Int): Stream[Int] =
        cons(pprev, go(prev, pprev + prev))

      go(0, 1)
    }
  }

}

object ex5_11 {

  import Stream._

  sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Cons(hd, tl) => hd() :: tl().toList
      case Empty => List[A]()
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(hd, tl) if n > 1 => cons(hd(), tl().take(n - 1))
      case Cons(hd, _) if n == 1 => cons(hd(), empty)
      case Empty => empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, tl) if n > 0 => tl().drop(n - 1)
      case _ => this
    }

    //    def takeWhile(p: A => Boolean): Stream[A] = this match {
    //      case Cons(hd, tl) if p(hd()) => cons(hd(), tl().takeWhile(p))
    //      case _ => empty
    //    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    def takeWhile(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) => if (!p(a)) empty else cons(a, b))

    def headOption: Option[A] =
      foldRight(None: Option[A])((a, _) => Some(a))

    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B])((a, b) => cons(f(a), b))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B])((a, b) => b.append(f(a)))

    def filter(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

    def append[AA >: A](that: => Stream[AA]): Stream[AA] =
      foldRight(that)((a, b) => cons(a, b))

  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = {
        println("saving head")
        hd
      }
      lazy val tail = {
        println("saving tail")
        tl
      }
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    //0, 1, 1, 2, 3, 5, 8
    def fibs: Stream[Int] = {
      def go(pprev: Int, prev: Int): Stream[Int] =
        cons(pprev, go(prev, pprev + prev))

      go(0, 1)
    }

    //It takes an initial state, and a function for producing both the next state and the next value in the generated stream.
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) match {
        case None => empty[A]
        case Some((nextValue, nextState)) => cons(nextValue, unfold(nextState)(f))
      }

  }

}

object ex5_12 {

  import Stream._

  sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Cons(hd, tl) => hd() :: tl().toList
      case Empty => List[A]()
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(hd, tl) if n > 1 => cons(hd(), tl().take(n - 1))
      case Cons(hd, _) if n == 1 => cons(hd(), empty)
      case Empty => empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, tl) if n > 0 => tl().drop(n - 1)
      case _ => this
    }

    //    def takeWhile(p: A => Boolean): Stream[A] = this match {
    //      case Cons(hd, tl) if p(hd()) => cons(hd(), tl().takeWhile(p))
    //      case _ => empty
    //    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    def takeWhile(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) => if (!p(a)) empty else cons(a, b))

    def headOption: Option[A] =
      foldRight(None: Option[A])((a, _) => Some(a))

    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B])((a, b) => cons(f(a), b))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B])((a, b) => b.append(f(a)))

    def filter(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

    def append[AA >: A](that: => Stream[AA]): Stream[AA] =
      foldRight(that)((a, b) => cons(a, b))

  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = {
        println("saving head")
        hd
      }
      lazy val tail = {
        println("saving tail")
        tl
      }
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

    //    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    //    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    //0, 1, 1, 2, 3, 5, 8
    //    def fibs:Stream[Int] = {
    //      def go(pprev: Int, prev: Int): Stream[Int] =
    //        cons(pprev, go(prev, pprev + prev))
    //      go(0,1)
    //    }

    //It takes an initial state, and a function for producing both the next state and the next value in the generated stream.
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) match {
        case None => empty
        case Some((nextValue, nextState)) => cons(nextValue, unfold(nextState)(f))
      }

    def constant[A](a: A): Stream[A] = unfold(a)(s => Some((s, s)))

    def from(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

    def ones: Stream[Int] = unfold(1)(s => Some(s, s))

    def fibs: Stream[Int] = unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))

  }

}

object ex5_13 {

  import Stream._

  sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Cons(hd, tl) => hd() :: tl().toList
      case Empty => List[A]()
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(hd, tl) if n > 1 => cons(hd(), tl().take(n - 1))
      case Cons(hd, _) if n == 1 => cons(hd(), empty)
      case Empty => empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, tl) if n > 0 => tl().drop(n - 1)
      case _ => this
    }

    //    def takeWhile(p: A => Boolean): Stream[A] = this match {
    //      case Cons(hd, tl) if p(hd()) => cons(hd(), tl().takeWhile(p))
    //      case _ => empty
    //    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    def takeWhile(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) => if (!p(a)) empty else cons(a, b))

    def headOption: Option[A] =
      foldRight(None: Option[A])((a, _) => Some(a))

    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B])((a, b) => cons(f(a), b))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B])((a, b) => b.append(f(a)))

    def filter(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

    def append[AA >: A](that: => Stream[AA]): Stream[AA] =
      foldRight(that)((a, b) => cons(a, b))

  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = {
        println("saving head")
        hd
      }
      lazy val tail = {
        println("saving tail")
        tl
      }
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

    //    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    //    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    //0, 1, 1, 2, 3, 5, 8
    //    def fibs:Stream[Int] = {
    //      def go(pprev: Int, prev: Int): Stream[Int] =
    //        cons(pprev, go(prev, pprev + prev))
    //      go(0,1)
    //    }

    //It takes an initial state, and a function for producing both the next state and the next value in the generated stream.
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) match {
        case None => empty
        case Some((nextValue, nextState)) => cons(nextValue, unfold(nextState)(f))
      }

    def constant[A](a: A): Stream[A] = unfold(a)(s => Some((s, s)))

    def from(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

    def ones: Stream[Int] = unfold(1)(s => Some(s, s))

    def fibs: Stream[Int] = unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = unfold(stream) {
      case Cons(hd, tl) => Some((f(hd()), tl()))
      case Empty => None
    }

    def takeWhile[A](stream: Stream[A])(p: A => Boolean): Stream[A] = unfold(stream) {
      case Cons(hd, tl) => if (p(hd())) Some((hd(), tl())) else None
      case Empty => None
    }

    def take[A](stream: Stream[A])(n: Int): Stream[A] = unfold((stream, n)) {
      case (Cons(hd, tl), nn) if nn > 1 => Some((hd(), (tl(), nn - 1)))
      case (Cons(hd, _), 1) => Some((hd(), (empty, 0)))
      case (Empty, _) => None
    }

    def zipWith[A, B, C](s1: Stream[A])(s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold(s1, s2) {
      case (Cons(hd1, tl1), Cons(hd2, tl2)) => Some((f(hd1(), hd2()), (tl1(), tl2())))
      case _ => None
    }

    def zipAll[A, B](s1: Stream[A])(s2: Stream[B]): Stream[(Option[A], Option[B])] =
      zipWithAll(s1)(s2)((_, _))

    def zipWithAll[A, B, C](s1: Stream[A])(s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
      unfold(s1, s2) {
        case (Empty, Empty) => None
        case (Cons(hd1, tl1), Cons(hd2, tl2)) => Some(f(Some(hd1()), Some(hd2())), (tl1(), tl2()))
        case (Empty, Cons(hd2, tl2)) => Some(f(None, Some(hd2())), (empty, tl2()))
        case (Cons(hd1, tl1), Empty) => Some(f(Some(hd1()), None), (tl1(), empty))
      }

  }

}

object ex5_14 {

  import Stream._

  sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Cons(hd, tl) => hd() :: tl().toList
      case Empty => List[A]()
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(hd, tl) if n > 1 => cons(hd(), tl().take(n - 1))
      case Cons(hd, _) if n == 1 => cons(hd(), empty)
      case Empty => empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, tl) if n > 0 => tl().drop(n - 1)
      case _ => this
    }

    //    def takeWhile(p: A => Boolean): Stream[A] = this match {
    //      case Cons(hd, tl) if p(hd()) => cons(hd(), tl().takeWhile(p))
    //      case _ => empty
    //    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    def takeWhile(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) => if (!p(a)) empty else cons(a, b))

    def headOption: Option[A] =
      foldRight(None: Option[A])((a, _) => Some(a))

    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B])((a, b) => cons(f(a), b))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B])((a, b) => b.append(f(a)))

    def filter(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

    def append[AA >: A](that: => Stream[AA]): Stream[AA] =
      foldRight(that)((a, b) => cons(a, b))

    def startsWith[AA >: A](s: Stream[AA]): Boolean =
      zipWithAll(this)(s){
        case (Some(a), Some(b)) => a == b
        case (None, _) => true
        case (_, None) => true
        case _ => false
      }.forAll(_ == true)



    //def hasSubsequences[A](sup: Stream[A], sub: Stream[A]): Boolean = ???

  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = {
        println("saving head")
        hd
      }
      lazy val tail = {
        println("saving tail")
        tl
      }
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

    //    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    //    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    //0, 1, 1, 2, 3, 5, 8
    //    def fibs:Stream[Int] = {
    //      def go(pprev: Int, prev: Int): Stream[Int] =
    //        cons(pprev, go(prev, pprev + prev))
    //      go(0,1)
    //    }

    //It takes an initial state, and a function for producing both the next state and the next value in the generated stream.
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) match {
        case None => empty
        case Some((nextValue, nextState)) => cons(nextValue, unfold(nextState)(f))
      }

    def constant[A](a: A): Stream[A] = unfold(a)(s => Some((s, s)))

    def from(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

    def ones: Stream[Int] = unfold(1)(s => Some(s, s))

    def fibs: Stream[Int] = unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = unfold(stream) {
      case Cons(hd, tl) => Some((f(hd()), tl()))
      case Empty => None
    }

    def takeWhile[A](stream: Stream[A])(p: A => Boolean): Stream[A] = unfold(stream) {
      case Cons(hd, tl) => if (p(hd())) Some((hd(), tl())) else None
      case Empty => None
    }

    def take[A](stream: Stream[A])(n: Int): Stream[A] = unfold((stream, n)) {
      case (Cons(hd, tl), nn) if nn > 1 => Some((hd(), (tl(), nn - 1)))
      case (Cons(hd, _), 1) => Some((hd(), (empty, 0)))
      case (Empty, _) => None
    }

    def zipWith[A, B, C](s1: Stream[A])(s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold(s1, s2) {
      case (Cons(hd1, tl1), Cons(hd2, tl2)) => Some((f(hd1(), hd2()), (tl1(), tl2())))
      case _ => None
    }

    def zipAll[A, B](s1: Stream[A])(s2: Stream[B]): Stream[(Option[A], Option[B])] =
      zipWithAll(s1)(s2)((_, _))

    def zipWithAll[A, B, C](s1: Stream[A])(s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
      unfold(s1, s2) {
        case (Empty, Empty) => None
        case (Cons(hd1, tl1), Cons(hd2, tl2)) => Some(f(Some(hd1()), Some(hd2())), (tl1(), tl2()))
        case (Empty, Cons(hd2, tl2)) => Some(f(None, Some(hd2())), (empty, tl2()))
        case (Cons(hd1, tl1), Empty) => Some(f(Some(hd1()), None), (tl1(), empty))
      }

  }

}

object ex5_15 {

  import Stream._

  sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Cons(hd, tl) => hd() :: tl().toList
      case Empty => List[A]()
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(hd, tl) if n > 1 => cons(hd(), tl().take(n - 1))
      case Cons(hd, _) if n == 1 => cons(hd(), empty)
      case Empty => empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, tl) if n > 0 => tl().drop(n - 1)
      case _ => this
    }

    //    def takeWhile(p: A => Boolean): Stream[A] = this match {
    //      case Cons(hd, tl) if p(hd()) => cons(hd(), tl().takeWhile(p))
    //      case _ => empty
    //    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    def takeWhile(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) => if (!p(a)) empty else cons(a, b))

    def headOption: Option[A] =
      foldRight(None: Option[A])((a, _) => Some(a))

    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B])((a, b) => cons(f(a), b))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B])((a, b) => b.append(f(a)))

    def filter(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

    def append[AA >: A](that: => Stream[AA]): Stream[AA] =
      foldRight(that)((a, b) => cons(a, b))

    def startsWith[AA >: A](s: Stream[AA]): Boolean =
      zipWithAll(this)(s){
        case (Some(a), Some(b)) => a == b
        case (None, _) => true
        case (_, None) => true
        case _ => false
      }.forAll(_ == true)

    def tails: Stream[Stream[A]] =
      cons(this,unfold(this){
        case Empty => None
        case Cons(_,tl) => Some(tl(), tl())
      })

    def hasSubsequences[AA >: A](sub: Stream[AA]): Boolean = tails.exists(_.startsWith(sub))

    //Stream(1,2,3).scanRight(0)(_+_)
    //Stream(1+2+3+0, 2+3+0, 3+0, 0)

    def scanRight[B](z:B)(f: (A, => B) => B):Stream[B] =
      foldRight(z, Stream(z))((a,acc) => {
        lazy val acc1 = acc
        val b = f(a, acc1._1)
        (b, cons(b, acc1._2))
      })._2

  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = {
        println("saving head")
        hd
      }
      lazy val tail = {
        println("saving tail")
        tl
      }
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

    //    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    //    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    //0, 1, 1, 2, 3, 5, 8
    //    def fibs:Stream[Int] = {
    //      def go(pprev: Int, prev: Int): Stream[Int] =
    //        cons(pprev, go(prev, pprev + prev))
    //      go(0,1)
    //    }

    //It takes an initial state, and a function for producing both the next state and the next value in the generated stream.
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) match {
        case None => empty
        case Some((nextValue, nextState)) => cons(nextValue, unfold(nextState)(f))
      }

    def constant[A](a: A): Stream[A] = unfold(a)(s => Some((s, s)))

    def from(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

    def ones: Stream[Int] = unfold(1)(s => Some(s, s))

    def fibs: Stream[Int] = unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = unfold(stream) {
      case Cons(hd, tl) => Some((f(hd()), tl()))
      case Empty => None
    }

    def takeWhile[A](stream: Stream[A])(p: A => Boolean): Stream[A] = unfold(stream) {
      case Cons(hd, tl) => if (p(hd())) Some((hd(), tl())) else None
      case Empty => None
    }

    def take[A](stream: Stream[A])(n: Int): Stream[A] = unfold((stream, n)) {
      case (Cons(hd, tl), nn) if nn > 1 => Some((hd(), (tl(), nn - 1)))
      case (Cons(hd, _), 1) => Some((hd(), (empty, 0)))
      case (Empty, _) => None
    }

    def zipWith[A, B, C](s1: Stream[A])(s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold(s1, s2) {
      case (Cons(hd1, tl1), Cons(hd2, tl2)) => Some((f(hd1(), hd2()), (tl1(), tl2())))
      case _ => None
    }

    def zipAll[A, B](s1: Stream[A])(s2: Stream[B]): Stream[(Option[A], Option[B])] =
      zipWithAll(s1)(s2)((_, _))

    def zipWithAll[A, B, C](s1: Stream[A])(s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
      unfold(s1, s2) {
        case (Empty, Empty) => None
        case (Cons(hd1, tl1), Cons(hd2, tl2)) => Some(f(Some(hd1()), Some(hd2())), (tl1(), tl2()))
        case (Empty, Cons(hd2, tl2)) => Some(f(None, Some(hd2())), (empty, tl2()))
        case (Cons(hd1, tl1), Empty) => Some(f(Some(hd1()), None), (tl1(), empty))
      }

  }

}


