
val projectName = "advent-2023"

name := projectName
version := "1.0.0"
scalaVersion := "2.13.6"
organization := "com.buzuli"
organizationName := "Buzuli Bytes"

// Set to false or remove if you want to show stubs as linking errors
//nativeLinkStubs := true
//enablePlugins(ScalaNativePlugin)

/*
lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "test",
  ).
  enablePlugins(AssemblyPlugin)
 */

// https://mvnrepository.com/artifact/com.typesafe.scala-logging/scala-logging
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4"

// https://mvnrepository.com/artifact/ch.qos.logback/logback-classic
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.7"

// https://mvnrepository.com/artifact/com.softwaremill.sttp.client/core
libraryDependencies += "com.softwaremill.sttp.client" %% "core" % "2.2.4"

// https://mvnrepository.com/artifact/com.typesafe.akka/akka-actor
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.6.8"

// https://mvnrepository.com/artifact/com.typesafe.akka/akka-stream
libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.6.8"

// https://mvnrepository.com/artifact/com.typesafe.akka/akka-http
libraryDependencies += "com.typesafe.akka" %% "akka-http" % "10.2.0"

// https://mvnrepository.com/artifact/com.typesafe.play/play-json
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.9.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % Test


// === Dependencies required by scala-native ===

// https://mvnrepository.com/artifact/org.ekrich/sjavatime
//libraryDependencies += "org.ekrich" % "sjavatime_native0.4_2.13" % "1.1.6"

// === === === === ===


// Helpful when testing (recommended by scalatest)
logBuffered in Test := false

// The single Java source acts as the entry point for our plugin
compileOrder := CompileOrder.ScalaThenJava

// Target Java SE 8
scalacOptions += "-target:jvm-8"
javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint")

val gitInfo = {
  import scala.sys.process._

  val gitHash: String = ("git rev-parse --verify HEAD" !!) trim
  val gitDirty: Boolean = "git diff --quiet" ! match {
    case 0 => false
    case _ => true
  }

  (gitHash, gitDirty)
}

def buildArtifactName(extension: String = ".jar") = {
  val (gitHash, gitDirty) = gitInfo
  val dirtyStr = if (gitDirty) "-dirty" else ""
  //val name = s"${projectName}-${prestoVersion}-${gitHash}${dirtyStr}-${dateTime}${extension}"
  val name = s"${projectName}-${version}-${gitHash}${dirtyStr}${extension}"
  //println(s"PRESTO_IAM_AUTH_ARTIFACT: ${name}")

  name
}

artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
  buildArtifactName(s".${artifact.extension}")
}

mainClass in assembly := Some("com.buzuli.advent.Main")

assemblyMergeStrategy in assembly := {
  case PathList(path, xs @ _*) if path.startsWith("jackson-") => MergeStrategy.last
  case PathList("META-INF", "Main-Class", "com.buzuli.advent.Main") => MergeStrategy.first
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case PathList("reference.conf") => MergeStrategy.concat
  case _ => MergeStrategy.first
}

//assemblyJarName in assembly := {
//  buildArtifactName()
//}
