ThisBuild / organization := "com.streese.scala2.redbook"
ThisBuild / scalaVersion := "2.13.5"
ThisBuild / version      := "0.1.0-SNAPSHOT"

lazy val redbook = (project in file("."))
  .settings(
    name                 := "code",
    libraryDependencies ++= Seq(libScalaTest)
  )

lazy val mdoc = (project in file("mdoc"))
  .settings(
    name          := "mdoc",
    mdocIn        := (ThisBuild / baseDirectory).value / "notesRaw",
    mdocOut       := (ThisBuild / baseDirectory).value / "notes",
    mdocVariables := Map("VERSION" -> version.value)
  )
  .dependsOn(redbook)
  .enablePlugins(MdocPlugin)

lazy val libScalaTest = "org.scalatest" %% "scalatest" % "3.2.6" % Test

ThisBuild / scapegoatVersion := "1.4.8"
