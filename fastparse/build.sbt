ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.6"

name := "sammy-fastparse"

scalafmtOnCompile := true

lazy val root = (project in file("."))
  .enablePlugins(ScalaNativePlugin)
  .settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "fastparse" % "3.1.1"
    )
  )
