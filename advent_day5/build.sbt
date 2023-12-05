val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent_of_code_day5",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "org.scala-lang" %% "toolkit" % "0.1.7"
  )
