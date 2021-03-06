name := "FooPar"

version := "0.1"

scalaVersion := "2.11.1"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.4"

libraryDependencies += "com.typesafe.akka" %% "akka-remote" % "2.3.4"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.4" % "test"

autoAPIMappings := true

scalacOptions in (Compile, doc) <++= baseDirectory.map {
      (bd: File) => Seq[String](
        "-sourcepath", bd.getAbsolutePath,
        "-doc-source-url",
"https://github.com/hejfelix/FooPar/tree/master€{FILE_PATH}.scala"
     )
   }

//scalacOptions in (Compile, doc) ++= Seq("-doc-source-url","/scala€{FILE_PATH}.scala")
