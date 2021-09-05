package com.rockthejvm.part2effects

import cats.effect.IO

import scala.io.StdIn

object IOIntroduction {

  // IO
  //pure : evaluates eagerly
  val ourFirstIO: IO[Int] = IO.pure(42) // arg that should not have side effects
  val aDelayedIO: IO[Int] = IO.delay {
    println("I'm producing an integer")
    54
  }

  val aDelayedIO_v2: IO[Int] = IO { // apply == delay
    println("I'm producing an integer")
    54
  }

  // map, flatMap
  val improvedMeaningOfLife = ourFirstIO.map(_ * 2)
  val printedMeaningOfLife = ourFirstIO.flatMap(mol => IO.delay(println(mol)))

  def smallProgram(): IO[Unit] =
    for {
      line1 <- IO(StdIn.readLine())
      line2 <- IO(StdIn.readLine())
      _ <- IO.delay(println(line1 + line2))
    } yield ()

  // mapN - combine IO effects as tuples

  import cats.syntax.apply._

  val combinedMeaningOfLife: IO[Int] =
    (ourFirstIO, improvedMeaningOfLife).mapN(_ + _)

  def smallProgram_v2(): IO[Unit] =
    (IO(StdIn.readLine()), IO(StdIn.readLine())).mapN(_ + _).map(println)

  /**
   * Exercises
   */
  //1 - Sequence two IOs and take the result of the LAST one
  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa.flatMap(_ => iob)

  def sequenceTakeLastv2[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa *> iob // andThen

  def sequenceTakeLastv3[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa >> iob // andThen by-name call of iob

  //2 - Sequence two IOs and take the result of the FIRST one
  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa.flatMap(a => iob.map(_ => a))

  def sequenceTakeFirstv2[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa <* iob //

  //3 - Repeat an IO effect forever
  def forever[A](ioa: IO[A]): IO[A] =
    ioa.flatMap(_ => forever(ioa))

  //lazy evaluation
  def foreverv2[A](ioa: IO[A]): IO[A] =
    ioa >> foreverv2(ioa)

  //eager evaluation - will lead to stack over flow even without the
  // call to unsafeRunSync
  def foreverv3[A](ioa: IO[A]): IO[A] =
    ioa *> foreverv3(ioa)

  def foreverv4[A](ioa: IO[A]): IO[A] =
    ioa.foreverM //with tail recursion

  //4 - Convert an IO to a different type
  def convert[A, B](ioa: IO[A], value: B): IO[B] =
    ioa.map(_ => value)

  def convertv2[A, B](ioa: IO[A], value: B): IO[B] =
    ioa.as(value)

  //5 - Discard value inside an IO and return Unit
  def asUnit[A](ioa: IO[A]): IO[Unit] = ioa.map(_ => ())

  def asUnitV2[A](ioa: IO[A]): IO[Unit] =
    ioa.void //ioa.as(())

  //6 - fix stack recursion
  def sum(n: Int): Int =
    if (n <= 0) 0
    else n + sum(n - 1)

  def sumIO(n: Int): IO[Int] = {
    def loop(accum: IO[Int], m: Int): IO[Int] =
      if (m <= 0) accum else loop(accum.map(_ + m), m - 1)

    loop(IO.pure(0), n)
  }

  def sumIOv2(n: Int): IO[Int] =
    if (n <= 0) IO(0)
    else
      for {
        lastNum <- IO(n)
        prevSum <- sumIOv2(n - 1)
      } yield lastNum + prevSum

  //7 - Write a fibonacci IO that does not stack overflow
  def fibonacci(n: Int): IO[BigInt] = {
    if (n <= 1) IO(1)
    else
      IO(fibonacci(n - 1))
        .flatMap(minusOneIO =>
          minusOneIO.flatMap(minusOne =>
            IO(fibonacci(n - 2)).flatMap(minusTwoIO =>
              minusTwoIO.map(minusTwo => minusTwo + minusOne)
            )
          )
        )
  }

  //same thing with for comprehension
  def fibonacciv2(n: Int): IO[BigInt] = {
    if (n <= 1) IO(1)
    else
      for {
        last <- IO(fibonacciv2(n - 1)).flatMap(identity) //or .flatten as below
        nextToLast <- IO(fibonacciv2(n - 2)).flatten
      } yield last + nextToLast
  }

  //Using IO.defer == IO.delay.flatten = suspending IO inside another IO
  def fibonacciv3(n: Int): IO[BigInt] = {
    if (n <= 1) IO(1)
    else
      for {
        last <- IO.defer(fibonacciv2(n - 1))
        nextToLast <- IO.defer(fibonacciv2(n - 2))
      } yield last + nextToLast
  }

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global // "platform"
    // "end of the world"
    //  (1 to 100).foreach(i => println(fibonacci(i).unsafeRunSync()))

    /*
      foreverv3 stack overflow without calling unsafeRunSync
     */
    /*foreverv3(IO {
      println("Forever")
      Thread.sleep(200)
    })*/

    //println(sumIO(20000).unsafeRunSync())
    //println(sum(20000))

    //Don't try over 35
    (1 to 30) foreach { i => println(s"$i :" + fibonacciv2(i).unsafeRunSync()) }
    //println(fibResult)
  }
}
