ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"
mainClass := Some("com.example.Hello")
lazy val root = (project in file("."))
  .settings(
    name := "paraProject"
  )
