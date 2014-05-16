name := "LetsBuildACompilerInScala"

version := "0.1"

scalaVersion := "2.10.2"

scalacOptions in ThisBuild ++= Seq("-feature")

mainClass in (Compile,run) := Some("Main")
