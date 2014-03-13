name := "dhek-integration"

scalaVersion := "2.10.3"

organization := "fr.applicius.dhek"

libraryDependencies += "org.scalaz"  %% "scalaz-core" % "7.1.0-M3"

libraryDependencies += "org.specs2" %% "specs2" % "2.3.2" % "test"

libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.3"

libraryDependencies += "org.eclipse.jetty" % "jetty-servlets" % "9.1.3.v20140225"

libraryDependencies += "org.eclipse.jetty" % "jetty-webapp" % "9.1.3.v20140225"

libraryDependencies += "org.eclipse.jetty" % "jetty-continuation" % "9.1.3.v20140225"

libraryDependencies += "io.argonaut" %% "argonaut" % "6.1-M2"

libraryDependencies += "com.itextpdf" % "itextpdf" % "5.5.0"

libraryDependencies += "org.bouncycastle" % "bcprov-jdk15on" % "1.50"

libraryDependencies += "org.bouncycastle" % "bcpkix-jdk15on" % "1.50"
