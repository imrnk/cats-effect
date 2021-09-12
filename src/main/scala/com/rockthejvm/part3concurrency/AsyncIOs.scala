package com.rockthejvm.part3concurrency

import cats.effect.{IO, IOApp}
import com.rockthejvm.utilsScala2._

import java.util.concurrent.Executors
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object AsyncIOs extends IOApp.Simple {

  // IOs can run asynchronously on fibers, without having to manually manage the fiber lifecycle
  val threadPool = Executors.newFixedThreadPool(8)
  implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutorService(threadPool)
  type Callback[A] = Either[Throwable, A] => Unit


  def computeMeaningOfLife(): Int = {
    Thread.sleep(1000)
    println(
      s"[${Thread.currentThread().getName}] computing the meaning of life on some other thread..."
    )
    42
  }

  def computeMeaningOfLifeEither(): Either[Throwable, Int] =
    Try {
      computeMeaningOfLife()
    }.toEither

  def computeMolOnThreadPool(): Unit =
    threadPool.execute(() => computeMeaningOfLife())

  // lift computation to an IO
  // async is a FFI
  val asyncMolIO: IO[Int] = IO.async_ {
    (cb: Callback[Int]) => // CE thread blocks (semantically) until this cb is invoked (by some other thread)
      threadPool.execute { () => // computation not managed by CE
        val result = computeMeaningOfLifeEither()
        cb(result) // CE thread is notified with the result
      }
  }

  /**
   * Exercise: lift an async computation on ec to an IO.
   */
  def asyncToIO[A](computation: () => A)(ec: ExecutionContext): IO[A] =
    IO.async_[A] { (cb: Callback[A]) =>
      ec.execute { () =>
        val result = Try(computation()).toEither
        cb(result)
      }
    }

  val asyncMolIO_v2: IO[Int] = asyncToIO(computeMeaningOfLife)(ec)

  /**
   * Exercise: lift an async computation as a Future to an IO
   */
  lazy val molFuture: Future[Int] = Future { computeMeaningOfLife }(ec)

  def convertFutureToIO[A](futComputation : => Future[A])(implicit ec : ExecutionContext) : IO[A] =
    IO.async_[A] {(cb: Callback[A]) =>
      ec.execute { () =>
       val result =  futComputation.onComplete { tryResult =>
         val result =  tryResult.toEither
         cb(result)
        }
      }
    }

  val asyncMolIO_v3: IO[Int] = convertFutureToIO(molFuture)(ec)
  //CE API
  val asyncMolIO_v4: IO[Int] = IO.fromFuture(IO(molFuture))

  /**
   * Exercise: a never ending IO?
   */
    def neverEndingIO[A](computation: () => A)(implicit ec: ExecutionContext) : IO[A] =
      IO.async_ {(cb : Callback[A]) =>
        ec.execute { () =>
          computation()
        }
      }

  // Never ending IO :   IO.async_[Any](_ => ())
  val neverEndingIO_V2 : IO[Any] = IO.never

  val neverMolIO : IO[Int] = neverEndingIO(computeMeaningOfLife)(ec)

  /*
    FULL ASYNC CALL
   */
  def demoAsyncCancellation() = {
    val asyncMeaningOfLifeIO_v2 : IO[Int] = IO.async { (cb: Callback[Int]) =>
      /*
        finalizer in case computation gets cancelled.
        finalizers are of type IO[Unit]
        not specifying finalizer => Option[IO[Unit]]
        creating Option is an effect, therefore wrapped in an IO => IO[Option[IO[Unit]]]
       */
      //return IO[Option[IO[Unit]]]
      IO {
        threadPool.execute{ () =>
          val result = computeMeaningOfLifeEither()
          cb(result)
        }
      } //IO[Unit]
        .as(Some(IO("Cancelled!").debug.void)) //finalizer
    }

    for {
      fib <- asyncMeaningOfLifeIO_v2.start
      _ <- IO.sleep(2.seconds) >> IO("cancelling...").debug >> fib.cancel
      _ <- fib.join
    } yield ()
  }

  override def run = demoAsyncCancellation().debug >> IO(threadPool.shutdown())

    //neverMolIO.debug.void

    /*asyncMolIO_v4.debug >>
                     asyncMolIO_v3.debug >>
                      asyncMolIO_v2.debug >>
                     IO(threadPool.shutdown()) *///
}
