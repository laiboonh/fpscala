package chapter4

object ex4_1 {

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case Some(a) => Some(f(a))
      case None => None
    }

    def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(a) => a
      case None => default
    }

    def orElse[B >: A](option: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(option)

    def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

}

object ex4_2 {

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case Some(a) => Some(f(a))
      case None => None
    }

    def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(a) => a
      case None => default
    }

    def orElse[B >: A](option: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(option)

    def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  def variance(xs: Seq[Double]): Option[Double] = {
    val mean: Option[Double] = if (xs.length == 0) None else Some(xs.sum / xs.length)
    val deviations: Option[Seq[Double]] = mean.map(m => xs.map(x => math.pow(x - m, 2)))
    deviations.flatMap(devs => if (devs.length == 0) None else Some(devs.sum / devs.length))
  }
}

object ex4_3 {

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case Some(a) => Some(f(a))
      case None => None
    }

    def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(a) => a
      case None => default
    }

    def orElse[B >: A](option: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(option)

    def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

    def lift[A, B](f: A => B): Option[A] => Option[B] = (x) => x map f
  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???

  def map2[A, B, C](optA: Option[A], optB: Option[B])(f: (A, B) => C): Option[C] =
    optA.flatMap(a => optB.map(b => f(a, b)))

  def Try[A](a: => A): Option[A] =
    try
      Some(a)
    catch {
      case e: Exception => None
    }

}

object ex4_4 {

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case Some(a) => Some(f(a))
      case None => None
    }

    def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(a) => a
      case None => default
    }

    def orElse[B >: A](option: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(option)

    def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

    def lift[A, B](f: A => B): Option[A] => Option[B] = (x) => x map f
  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???

  def map2[A, B, C](optA: Option[A], optB: Option[B])(f: (A, B) => C): Option[C] =
    optA.flatMap(a => optB.map(b => f(a, b)))

  def Try[A](a: => A): Option[A] =
    try
      Some(a)
    catch {
      case e: Exception => None
    }

  def sequence[A](optionList: List[Option[A]]): Option[List[A]] = {
    def go(optionList: List[Option[A]], acc: Option[List[A]]): Option[List[A]] = optionList match {
      case Some(a) :: tl => go(tl, acc.map(_ :+ a))
      case Nil => acc
      case None :: _ => None
    }

    go(optionList, Some(List.empty[A]))
  }


}

object ex4_5 {

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case Some(a) => Some(f(a))
      case None => None
    }

    def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(a) => a
      case None => default
    }

    def orElse[B >: A](option: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(option)

    def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

    def lift[A, B](f: A => B): Option[A] => Option[B] = (x) => x map f
  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???

  def map2[A, B, C](optA: Option[A], optB: Option[B])(f: (A, B) => C): Option[C] =
    optA.flatMap(a => optB.map(b => f(a, b)))

  def Try[A](a: => A): Option[A] =
    try
      Some(a)
    catch {
      case e: Exception => None
    }

  def sequence[A](optionList: List[Option[A]]): Option[List[A]] = {
    def go(optionList: List[Option[A]], acc: Option[List[A]]): Option[List[A]] = optionList match {
      case Some(a) :: tl => go(tl, acc.map(_ :+ a))
      case Nil => acc
      case None :: _ => None
    }

    go(optionList, Some(List.empty[A]))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(List.empty[B])
    case hd::tl => f(hd) match {
      case None => None
      case Some(a) => traverse(tl)(f).map(a +: _)
    }
  }


}