name := "Ethan Garrison Advent of Code"

scalaVersion := "2.13.1"

scalacOptions := Seq("-unchecked", "-deprecation")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.1.0" % "test"
)

logBuffered in Test := false
