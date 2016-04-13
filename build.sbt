import sbt._

name := "immutable-fixture"

version := "1.0"

scalaVersion := "2.11.8"

resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "org.typelevel" %% "cats" % "0.4.0"
libraryDependencies += "org.zalando" %% "scala-jsonapi" % "0.3.4"
libraryDependencies += "org.specs2" %% "specs2-core" % "3.7.2" % "test"
libraryDependencies ++= Seq("monocle-core", "monocle-generic", "monocle-macro", "monocle-state", "monocle-refined")
  .map("com.github.julien-truffaut" %% _ % "1.2.0" % "test")

scalacOptions in Test ++= Seq("-Yrangepos", "-feature")