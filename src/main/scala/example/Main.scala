package example

import cats.effect.IO

object HelloWorld {

  def double(i: Int): Int = i * 2

  def sum(list: List[Int]): Int = list match {
    case Nil => 0
    case head :: tail => head + sum(tail)
  }

  def toInt(s: String): Option[Int] = {
    try {
      Some(Integer.parseInt(s.trim))
    } catch {
      case e: Exception => None
    }
  }

  def test(x: String): Int = {
    toInt(x) match {
      case Some(i) => i
      case None => 0
    }
  }

  class Person {
    var name: Option[String] = None
    var age: Option[Int] = None

    override def toString = s"$name, $age"
  }

  object Person {
    // a one-arg constructor
    def apply(name: Option[String]): Person = {
      var p = new Person
      p.name = name
      p
    }

    // a two-arg constructor
    def apply(name: Option[String], age: Option[Int]): Person = {
      var p = new Person
      p.name = name
      p.age = age
      p
    }
  }

  trait Person2 {
    def name: String
  }

  case class Student(name: String, year: Int) extends Person2

  case class Teacher(name: String, specialty: String) extends Person2

  def fib(n: Int, a: Long = 0, b: Long = 1): IO[Long] =
    IO(a + b).flatMap { b2 =>
      if (n > 0)
        fib(n - 1, b, b2)
      else
        IO.pure(a)
    }

  def fac(x: Int, p: Int = 1): IO[Int] = {
    IO(x * p).flatMap { t =>
      if (x > 1) {
        fac(x - 1, t)
      } else {
        IO.pure(t)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    //    println("Hello, world!")
    //    val d = double(2)
    //    println(s"d=\"$d\"")
    //    val list = List(1, 2, 3)
    //    val s = sum(list)
    //    println(s"sum=\"$s\"")
    //    println(s"===")
    //    toInt("12") match {
    //      case Some(i) => println(i)
    //      case None => println("does not work")
    //    }
    //    val stringA = "a"
    //    val stringB = "2"
    //    val stringC = "3"
    //    val y = for {
    //      a <- toInt(stringA)
    //      b <- toInt(stringB)
    //      c <- toInt(stringC)
    //    } yield a + b + c
    //    println(s"y=$y")
    //    val m = test("a")
    //    println(s"m=$m")
    //    val n = test("10")
    //    println(s"n=$n")
    //    val p = Person(None)
    //    println(p.name.getOrElse("no name yet"))
    //    val p2 = Person(Some("max"))
    //    println(p2.name.getOrElse("no name yet"))

    val a = fib(0).flatMap(n => IO(println(s"Number is: $n")))
    val b = fib(1).flatMap(n => IO(println(s"Number is: $n")))
    val c = fib(2).flatMap(n => IO(println(s"Number is: $n")))
    val d = fib(3).flatMap(n => IO(println(s"Number is: $n")))

    a.unsafeRunSync()
    b.unsafeRunSync()
    c.unsafeRunSync()
    d.unsafeRunSync()

    val e = fac(1).flatMap(n => IO(println(s"1's factorial is: $n")))
    val f = fac(2).flatMap(n => IO(println(s"2's factorial is: $n")))
    val g = fac(10).flatMap(n => IO(println(s"10's factorial is: $n")))

    e.unsafeRunSync()
    f.unsafeRunSync()
    g.unsafeRunSync()
  }

}