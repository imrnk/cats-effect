package com.rockthejvm.part3concurrency

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{IO, IOApp, Resource}

import java.io.{File, FileReader}
import java.util.Scanner
import scala.concurrent.duration._

object Resources extends IOApp.Simple {

  import com.rockthejvm.utilsScala2._

  // use-case: manage a connection lifecycle
  class Connection(url: String) {
    def open(): IO[String] = IO(s"opening connection to $url").debug

    def close(): IO[String] = IO(s"closing connection to $url").debug
  }

  val asyncFetchUrl = for {
    fib <- (new Connection("rockthejvm.com").open() *> IO.sleep(
      (Int.MaxValue).seconds
    )).start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()
  // problem: leaking resources

  val correctAsyncFetchUrl = for {
    conn <- IO(new Connection("rockthejvm.com"))
    fib <- (conn.open() *> IO.sleep((Int.MaxValue).seconds))
      .onCancel(conn.close().void)
      .start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  /*
    bracket pattern: someIO.bracket(useResourceCb)(releaseResourceCb)
    bracket is equivalent to try-catches (pure FP)
   */
  val bracketFetchUrl = IO(new Connection("rockthejvm.com"))
    .bracket(conn => conn.open() *> IO.sleep(Int.MaxValue.seconds))(conn =>
      conn.close().void
    )

  val bracketProgram = for {
    fib <- bracketFetchUrl.start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  /**
   * Exercise: read the file with the bracket pattern
   *  - open a scanner
   *  - read the file line by line, every 100 millis
   *  - close the scanner
   *  - if cancelled/throws error, close the scanner
   */
  def openFileScanner(path: String): IO[Scanner] =
    IO(new Scanner(new FileReader(new File(path))))

  def readLineByLine(scanner: Scanner): IO[Unit] =
    if (scanner.hasNextLine)
      IO(scanner.nextLine()).debug >>
      IO.sleep(100.millis) >>
      readLineByLine(scanner)
    else IO.unit

  def bracketReadFile(path: String): IO[Unit] = {
    IO(s"Opening file at $path") *>
    openFileScanner(path).bracket { scanner =>
      readLineByLine(scanner)
    } { scanner =>
      IO(s"Closing file at $path").debug *> IO(scanner.close())
    }
  }

  /**
   * Resources
   */
  def connFromConfig(path: String): IO[Unit] =
    openFileScanner(path).bracket { scanner =>
      IO(new Connection(scanner.nextLine())).bracket { conn =>
        conn.open() >> IO.never
      }(conn => conn.close().void)
    } { scanner => IO("closing file").debug >> IO(scanner.close()) }

  val connectionResources =
    Resource.make(IO(new Connection("rockthejvm.com")))(conn =>
      conn.close().void
    )

  val resourceFetchUrl = for {
    fib <- connectionResources.use(conn => conn.open() >> IO.never).start
    _ <- IO.sleep(1.second) >> fib.cancel
  } yield ()

  //resources are equivalent to brackets
  val simpleResource = IO("some resource")
  val usingResource: String => IO[String] = string =>
    IO(s"using the resouce $string").debug
  val releaseResource: String => IO[Unit] = string =>
    IO(s"releasing the resource $string").debug.void

  val usingResourceWithBracket =
    simpleResource.bracket(usingResource)(releaseResource)

  val usingResourceWithResource =
    Resource.make(simpleResource)(releaseResource).use(usingResource)

  /**
   * Exercise: read a text file with one line every 100 millis, implicit Resource
   * (refactor the bracket exercise to use Resource)
   */
  def resourceReadFile(path: String): IO[Unit] =
    Resource
      .make(openFileScanner(path))(scanner =>
        IO(s"Closing file at $path").debug *> IO(scanner.close())
      )
      .use(readLineByLine)

  def cancelReadFile(path: String): IO[Unit] =
    for {
      fib <- resourceReadFile(path).start
      _ <- IO.sleep(2.seconds) >> fib.cancel
    } yield ()

  //nested resources
  def connectionFromConfigurationResource(
                                           path: String
                                         ): Resource[IO, Connection] =
    Resource
      .make(
        IO(s"Opening file $path").debug >>
        openFileScanner(path)
      )(scanner => IO(s"closing file $path").debug >> IO(scanner.close()))
      .flatMap(scanner =>
        Resource.make(IO(new Connection(scanner.nextLine())))(conn =>
          conn.close().void
        )
      )

  //without flatMap used for comprehension for nested resource (Scanner -> Connection)
  def connectionFromConfigurationResource_V2(path: String) =
    for {
      scanner <- Resource.make(
        IO(s"Opening file $path").debug >> openFileScanner(path)
      )(scanner => IO(s"closing file $path").debug >> IO(scanner.close()))
      conn <- Resource.make(IO(new Connection(scanner.nextLine())))(conn =>
        conn.close().void
      )
    } yield conn

  val openConnection: IO[Unit] = connectionFromConfigurationResource(
    "src/main/resources/connection.txt"
  ).use(conn => conn.open() >> IO.never)
  //connection + file will be released automatically

  val readConnectionByResource = for {
    fib <- openConnection.start
    _ <- IO.sleep(2.seconds) >> fib.cancel
  } yield ()

  //finalizer to regular IOs
  val ioWithFinalizer =
    IO("some resource").debug.guarantee(IO("freeing resource").debug.void)

  val ioWithFinalizer_v2 = IO("some resource").debug.guaranteeCase {
    case Succeeded(fa) =>
      fa.flatMap(result => IO(s"releasing resource $result").debug).void
    case Errored(e)    => IO(s"nothing to release").debug.void
    case Canceled()    =>
      IO(s"resource got cancelled, releasing what's left").debug.void
  }

  override def run = ioWithFinalizer_v2.void
  //readConnectionByResource
  /*cancelReadFile(
      "src/main/scala/com/rockthejvm/part3concurrency/Resources.scala"
    )*/
  /*resourceReadFile(
      "src/main/scala/com/rockthejvm/part3concurrency/Resources.scala"
    )*/
  //resourceFetchUrl
  /*bracketReadFile(
      "src/main/scala/com/rockthejvm/part3concurrency/Resources.scala"
    )*/
}
