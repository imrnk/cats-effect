package com.rockthejvm.part2effects

import cats.effect.{IO, IOApp}

import scala.concurrent.Future
import scala.util.Random

object IOTraversal extends IOApp.Simple {

  import scala.concurrent.ExecutionContext.Implicits.global

  def heavyComputation(string: String): Future[Int] =
    Future {
      Thread.sleep(Random.nextInt(1000))
      string.split(" ").length
    }

  val workLoad: List[String] = List(
    "I quite like CE",
    "Scala is great",
    "looking forward to some awesome stuff"
  )

  def clunkyFutures(): Unit = {
    val futures: List[Future[Int]] = workLoad.map(heavyComputation)
    // Future[List[Int]] would be hard to obtain
    futures.foreach(_.foreach(println))
  }

  import cats.Traverse
  import cats.instances.list._
  val listTraverse = Traverse[List]

  def traverseFutures(): Unit = {
    // traverse
    val singleFuture: Future[List[Int]] =
      listTraverse.traverse(workLoad)(heavyComputation)
    // ^^ this stores ALL the results
    singleFuture.foreach(println)
  }

  import com.rockthejvm.utilsScala2._

  // traverse for IO
  def computeAsIO(string: String): IO[Int] =
    IO {
      Thread.sleep(Random.nextInt(1000))
      string.split(" ").length
    }.debug

  val ios: List[IO[Int]] = workLoad.map(computeAsIO)
  val singleIO: IO[List[Int]] = listTraverse.traverse(workLoad)(computeAsIO)

  // parallel traversal
  import cats.syntax.parallel._ // parTraverse extension method
  val parallelSingleIO: IO[List[Int]] = workLoad.parTraverse(computeAsIO)

  /**
    * Exercises
    */
  // hint: use Traverse API
  def sequence[A](listOfIOs: List[IO[A]]): IO[List[A]] = {
    val listTraverse = Traverse[List]
    listTraverse.traverse(listOfIOs)(ioa => ioa.map(identity))
  }

  // hard version
  def sequence_v2[F[_]: Traverse, A](listOfIOs: F[IO[A]]): IO[F[A]] = {
    val fTraverse = Traverse[F]
    fTraverse.traverse(listOfIOs)(ioa => ioa.map(identity))
  }

  // parallel version
  def parSequence[A](listOfIOs: List[IO[A]]): IO[List[A]] = {
    listOfIOs.parTraverse(ioa => ioa.map(identity))
  }

  // hard version
  def parSequence_v2[F[_]: Traverse, A](listOfIOs: F[IO[A]]): IO[F[A]] =
    listOfIOs.parTraverse(ioa => ioa.map(identity))

  val parSequenceV2IIO = parSequence_v2(workLoad.map(computeAsIO(_)))
  // existing sequence API

  // parallel sequencing

  override def run =
    //parallelSingleIO.map(_.sum).debug.void
    parSequenceV2IIO.map(_.sum).debug.void
}
