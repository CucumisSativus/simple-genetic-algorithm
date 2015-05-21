
lazy val commonSettings = Seq(
  organization := "com.example",
  version := "0.1.0",
  scalaVersion := "2.11.4"
  )
lazy val root = (project in file(".")).
  settings(commonSettings: _*).
    settings(
      name := "simple-genetic-algorithm",
      libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
    )

