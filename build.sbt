val scala3Version = "3.0.0"
val scala2Version = "2.12.10"

lazy val root = project
  .in(file("."))
  .settings(
    name := "cats-effect",
    version := "0.1.0",
    scalaVersion := scala2Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.2.0"
    )
  )
