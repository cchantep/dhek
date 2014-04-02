name := "dhek-integration"

scalaVersion := "2.10.3"

organization := "fr.applicius.dhek"

resolvers ++= Seq(
  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
  "Typesafe repository releases" at "http://repo.typesafe.com/typesafe/releases/",
  "Applicius Snapshots" at "https://raw.github.com/applicius/mvn-repo/master/snapshots/")

autoCompilerPlugins := true

addCompilerPlugin("org.eu.acolyte" %% "scalac-plugin" % "1.0.16")

//scalacOptions += "-P:acolyte:debug"

libraryDependencies ++= Seq(
  "org.scalaz"  %% "scalaz-core" % "7.1.0-M6",
  "org.specs2" %% "specs2" % "2.3.2" % "test",
  "com.jsuereth" %% "scala-arm" % "1.3",
  "org.eclipse.jetty" % "jetty-servlets" % "9.1.3.v20140225",
  "org.eclipse.jetty" % "jetty-webapp" % "9.1.3.v20140225",
  "org.eclipse.jetty" % "jetty-continuation" % "9.1.3.v20140225",
  "io.argonaut" %% "argonaut" % "6.1-M2",
  "com.itextpdf" % "itextpdf" % "5.5.0",
  "org.bouncycastle" % "bcprov-jdk15on" % "1.50",
  "org.bouncycastle" % "bcpkix-jdk15on" % "1.50",
  "org.reactivemongo" %% "reactivemongo" % "0.10.0",
  "com.typesafe" % "config" % "1.0.0",
  "commons-codec" % "commons-codec" % "1.7",
  "scaptcha" %% "scaptcha" % "1.1-SNAPSHOT"
)

