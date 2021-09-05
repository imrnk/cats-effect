package com.rockthejvm.part3concurrency

import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Fiber, IO, IOApp}

import scala.concurrent.duration.{FiniteDuration, _}

object RacingIOs extends IOApp.Simple {

  import com.rockthejvm.utilsScala2._

  def runWithSleep[A](value: A, duration: FiniteDuration): IO[A] =
    (
      IO(s"starting computation: $value").debug >>
      IO.sleep(duration) >>
      IO(s"computation for $value: done") >>
      IO(value)
      ).onCancel(IO(s"computation CANCELED for $value").debug.void)

  def testRace() = {
    val meaningOfLife = runWithSleep(42, 1.second)
    val favLang = runWithSleep("Scala", 2.seconds)
    val first: IO[Either[Int, String]] = IO.race(meaningOfLife, favLang)
    /*
      - both IOs run on separate fibers
      - the first one to finish will complete the result
      - the loser will be canceled
     */

    first.flatMap {
      case Left(mol)   => IO(s"Meaning of life won: $mol")
      case Right(lang) => IO(s"Fav language won: $lang")
    }
  }

  def testRacePair() = {
    val meaningOfLife = runWithSleep(42, 1.second)
    val favLang = runWithSleep("Scala", 2.seconds)
    val raceResult: IO[Either[
      (
        Outcome[IO, Throwable, Int],
          Fiber[IO, Throwable, String]
        ), // (winner result, loser fiber)
      (
        Fiber[IO, Throwable, Int],
          Outcome[IO, Throwable, String]
        ) // (loser fiber, winner result)
    ]] = IO.racePair(meaningOfLife, favLang)

    raceResult.flatMap {
      case Left((outMol, fibLang))  =>
        fibLang.cancel >> IO("MOL won").debug >> IO(outMol).debug
      case Right((fibMol, outLang)) =>
        fibMol.cancel >> IO("Language won").debug >> IO(outLang).debug
    }
  }

  /**
   * Exercises:
   * 1 - implement a timeout pattern with race
   * 2 - a method to return a LOSING effect from a race (hint: use racePair)
   * 3 - implement race in terms of racePair
   */
  // 1
  def timeOut[A](io: IO[A], duration: FiniteDuration): IO[A] =
    IO.race(io, IO.sleep(duration)).flatMap {
      case Left(value)    => IO(value)
      case Right(elapsed) => IO.raiseError(new RuntimeException(s"$elapsed time passed."))
    }

  private val importantTask = IO.sleep(2.seconds) >> IO(42)
  val testTimeout = timeOut(importantTask, 1.second)
  //cats effect api
  val testTimout_v2 = importantTask.timeout(1.second)

  //2
  def unrace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] = {
    val raceResult: IO[Either[(Outcome[IO, Throwable, A],
      Fiber[IO, Throwable, B]),
      (Fiber[IO, Throwable, A],
        Outcome[IO, Throwable, B])]] = IO.racePair(ioa, iob)

    raceResult.flatMap {
      case Left((outcomeA, fiberB))  => IO(s"Ignoring $outcomeA") >> IO("B loses").debug >> fiberB.join.flatMap {
        case Succeeded(fb) => fb.map(b => Right(b))
        case Errored(e)    => IO.raiseError(e)
        case Canceled()    => IO.raiseError(new RuntimeException("B cancelled"))
      }
      case Right((fiberA, outcomeB)) => IO(s"Ignoring $outcomeB") >> IO("A loses").debug >> fiberA.join.flatMap {
        case Succeeded(fa) => fa.map(a => Left(a))
        case Errored(e)    => IO.raiseError(e)
        case Canceled()    => IO.raiseError(new RuntimeException("A cancelled"))
      }
    }
  }

  def testUnrace() = {
    val meaningOfLife = runWithSleep(42, 1.second)
    val favLang = runWithSleep("Scala", 2.seconds)
    val first: IO[Either[Int, String]] = unrace(meaningOfLife, favLang)
    /*
      - both IOs run on separate fibers
      - the first one to finish will complete the result
      - the loser will be canceled
     */

    first.flatMap {
      case Left(mol)   => IO(s"Meaning of life won: $mol")
      case Right(lang) => IO(s"Fav language won: $lang")
    }
  }

  //3
  def simpleRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap {
      case Left((outcomeA, fibB))  => outcomeA match {
        case Succeeded(ioa) => fibB.cancel >> ioa.map(Left(_))
        case Errored(e)     => fibB.cancel >> IO.raiseError(e)
        case Canceled()     => fibB.join flatMap {
          case Succeeded(iob) => iob.map(Right(_))
          case Errored(e)     => IO.raiseError(e)
          case Canceled()     => IO.raiseError(new RuntimeException("Both computation cancelled"))
        }
      }
      case Right((fibA, outcomeB)) => outcomeB match {
        case Succeeded(iob) => fibA.cancel >> iob.map(Right(_))
        case Errored(e)     => fibA.cancel >> IO.raiseError(e)
        case Canceled()     => fibA.join flatMap {
          case Succeeded(ioa) => ioa.map(Left(_))
          case Errored(e)     => IO.raiseError(e)
          case Canceled()     => IO.raiseError(new RuntimeException("Both computation cancelled"))
        }
      }
    }

  override def run = testUnrace().debug.void
}
