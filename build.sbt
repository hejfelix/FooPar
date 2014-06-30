name := "FooPar"

version := "0.1"

scalaVersion := "2.11.1"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.4"

libraryDependencies += "com.typesafe.akka" %% "akka-remote" % "2.3.4"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.4" % "test"

