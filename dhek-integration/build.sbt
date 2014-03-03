name := "dhek-integration"

scalaVersion := "2.10.3"

organization := "fr.applicius.dhek"

libraryDependencies += "org.scalaz"  %% "scalaz-core" % "7.0.4"

libraryDependencies += "org.specs2" %% "specs2" % "2.3.2" % "test"

libraryDependencies += "net.databinder" %% "unfiltered-netty-server" % "0.7.1"
