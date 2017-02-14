import _root_.xerial.sbt.Pack._

import sbt.project

name := "scala-bandits"

organization := "be.ac.vub"

scalaVersion := "2.11.7"

//conflictManager := ConflictManager.strict

libraryDependencies ++= Seq(
  "eu.timepit" %% "refined" % "0.6.0",
  "org.scalanlp" %% "breeze" % "0.12",
  "org.scalanlp" %% "breeze-natives" % "0.12",
  "org.jfree" % "jfreechart" % "1.0.14",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test")

packAutoSettings

scalaSource in Compile := baseDirectory.value / "src"
scalaSource in Test := baseDirectory.value / "test-src"

version := "0.0.1"