name := "FingerTree"

version := "0.20-SNAPSHOT"

organization := "de.sciss"

description := "A Scala implementation of the versatile purely functional data structure of the same name."

homepage := Some( url( "https://github.com/Sciss/FingerTree" ))

scalaVersion := "2.9.1"

scalacOptions ++= Seq( "-deprecation", "-unchecked" )

initialCommands in console := """import de.sciss.fingertree._"""

libraryDependencies ++= Seq(
   "org.scalatest" %% "scalatest" % "1.7.1" % "test"
)
