ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name                             := "advent-of-code-2022",
    libraryDependencies += "dev.zio" %% "zio"         % "2.0.4",
    libraryDependencies += "dev.zio" %% "zio-streams" % "2.0.4",
    fork                             := true,
  )
