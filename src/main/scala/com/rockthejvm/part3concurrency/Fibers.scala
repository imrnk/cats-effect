package com.rockthejvm.part3concurrency

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Fiber, IO, IOApp, Outcome}
import scala.concurrent.duration._

object Fibers extends IOApp.Simple {

  val meaningOfLife = IO.pure(42)
  val favLang = IO.pure("Scala")

  import com.rockthejvm.utilsScala2._

  def sameThreadIOs() =
    for {
      _ <- meaningOfLife.debug
      _ <- favLang.debug
    } yield ()

  // introducing Fiber: a data structure describing an effect running on some thread
  def createFiber: Fiber[IO, Throwable, String] =
    ??? // almost impossible to create fibers manually

  // the fiber is not actually started, but the fiber allocation is wrapped in another effect
  val aFiber: IO[Fiber[IO, Throwable, Int]] = meaningOfLife.debug.start

  def differentThreadIOs() =
    for {
      _ <- aFiber
      _ <- favLang.debug
    } yield ()

  // joining a fiber
  def runOnSomeOtherThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] =
    for {
      fib <- io.start
      result <- fib.join // an effect which waits for the fiber to terminate
    } yield result
  /*
    possible outcomes:
    - success with an IO
    - failure with an exception
    - cancelled
   */

  val someIOOnAnotherThread = runOnSomeOtherThread(meaningOfLife)
  val someResultFromAnotherThread = someIOOnAnotherThread.flatMap {
    case Succeeded(effect) => effect
    case Errored(e)        => IO(0)
    case Canceled()        => IO(0)
  }

  def throwOnAnotherThread() =
    for {
      fib <- IO.raiseError[Int](new RuntimeException("no number for you")).start
      result <- fib.join
    } yield result

  def testCancel() = {
    val task = IO("starting").debug >> IO.sleep(1.second) >> IO("done").debug
    // onCancel is a "finalizer", allowing you to free up resources in case you get canceled
    val taskWithCancellationHandler =
      task.onCancel(IO("I'm being cancelled!").debug.void)

    for {
      fib <- taskWithCancellationHandler.start // on a separate thread
      _ <- IO.sleep(500.millis) >> IO(
        "cancelling"
      ).debug // running on the calling thread
      _ <- fib.cancel
      result <- fib.join
    } yield result
  }

  /**
   * Exercises:
   *  1. Write a function that runs an IO on another thread, and, depending on the result of the fiber
   *    - return the result in an IO
   *    - if errored or cancelled, return a failed IO
   *
   * 2. Write a function that takes two IOs, runs them on different fibers and returns an IO with a tuple containing both results.
   *    - if both IOs complete successfully, tuple their results
   *    - if the first IO returns an error, raise that error (ignoring the second IO's result/error)
   *    - if the first IO doesn't error but second IO returns an error, raise that error
   *    - if one (or both) canceled, raise a RuntimeException
   *
   * 3. Write a function that adds a timeout to an IO:
   *    - IO runs on a fiber
   *    - if the timeout duration passes, then the fiber is canceled
   *    - the method returns an IO[A] which contains
   *      - the original value if the computation is successful before the timeout signal
   *      - the exception if the computation is failed before the timeout signal
   *      - a RuntimeException if it times out (i.e. cancelled by the timeout)
   */
  // 1
  def processResultFromFiber[A](io: IO[A]): IO[A] = {
    val outcome = for {
      fib <- io.start
      result <- fib.join
    } yield result

    outcome flatMap {
      case Succeeded(effect) => effect
      case Errored(ex)       => IO.raiseError(ex)
      case Canceled()        => IO.raiseError(new RuntimeException("I'm cancelled"))
    }
  }

  def testEx1 = {
    val computation =
      IO("starting").debug >>
      IO.sleep(1.second) >>
      IO("done").debug >>
      IO(42).debug
    processResultFromFiber(computation).void

  }

  //2
  def tupleIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = {
    val result: IO[(Outcome[IO, Throwable, A], Outcome[IO, Throwable, B])] =
      for {
        fib1 <- ioa.start
        fib2 <- iob.start
        result1 <- fib1.join
        result2 <- fib2.join
      } yield (result1, result2)

    result flatMap {
      case (Succeeded(fa), Succeeded(fb)) =>
        fa.flatMap(a => fb.map(b => (a, b)))
      case (Errored(e), _)                => IO.raiseError(e)
      case (_, Errored(e))                => IO.raiseError(e)
      case _                              => IO.raiseError(new RuntimeException("Cancelled computation"))
    }
  }

  def testEx2 = {
    val ioa = IO("first").debug >> IO.sleep(2.second) >> IO(1).debug
    val iob = IO("second").debug >> IO.sleep(3.second) >> IO(2).debug

    tupleIOs(ioa, iob).debug.void
  }

  def testEx2_Cancelled = {
    val ioa = IO("first").debug >> IO.sleep(2.second) >> IO(1).debug
    val iob = IO("second").debug >> IO.sleep(1.second) >> IO.canceled.debug

    tupleIOs(ioa, iob).debug.void
  }

  //3
  def timeOut[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    val outcome = for {
      fib <- io.start
      _ <-
        IO.sleep(
          duration
        ) >> fib.cancel // If we start this (IO.sleep(duration) >> fib.cancel).start on diff fiber it can leak
      result <- fib.join
    } yield result
    outcome flatMap {
      case Succeeded(effect) => effect
      case Errored(e)        => IO.raiseError(e)
      case Canceled()        =>
        IO.raiseError(new RuntimeException("Cancelled by timeout"))
    }
  }

  override def run = {
    //testEx1
    testEx2
    //testEx2_Cancelled
    //timeOut(IO.sleep(2.seconds), 2.seconds).debug.void
    /*tupleIOs(
      IO(42),
      IO(10)
    ).debug.void*/
  }
}
