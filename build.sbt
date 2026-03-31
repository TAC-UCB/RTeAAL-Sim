organization := "io.github.ucsc-vama"

version := "0.8-SNAPSHOT"

name := "essent"

scalaVersion := "2.13.12"

scalacOptions ++= Seq("-deprecation", "-unchecked")

libraryDependencies += "com.github.scopt" %% "scopt" % "3.7.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test"

libraryDependencies += "org.json4s" %% "json4s-native" % "3.6.12"

libraryDependencies += "edu.berkeley.cs" %% "firrtl" % "1.5.6"

libraryDependencies += "org.msgpack" % "msgpack-core" % "0.8.22"

// libraryDependencies += "org.msgpack" %% "msgpack-scala" % "0.8.13"

// libraryDependencies += "org.msgpack" %% "msgpack-scala" % "0.6.8"


// libraryDependencies += "org.yaml" % "snakeyaml" % "1.29" 

val lihaoyiVersion = "0.9.1"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "ujson" % "3.1.3",
  "com.lihaoyi" %% "os-lib" % lihaoyiVersion
)

libraryDependencies += "com.lihaoyi" %% "upickle" % "3.1.3"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % "0.14.6",
  "io.circe" %% "circe-generic" % "0.14.6",
  "io.circe" %% "circe-parser" % "0.14.6"
)

javaOptions += "-Xms512m"
javaOptions += "-Xmx2g"


// Assembly

assembly / assemblyJarName := "essent.jar"

assembly / assemblyOutputPath:= file("./utils/bin/essent.jar")


// Ignore disabled .scala files
unmanagedSources / excludeFilter := HiddenFileFilter || "*disabled*.scala"



// Publishing setup
publishMavenStyle := true
Test / publishArtifact := false
pomIncludeRepository := { x => false }

// POM info
pomExtra := (
<url>https://github.com/ucsc-vama/essent</url>
<licenses>
  <license>
    <name>BSD-style</name>
    <url>hhttps://opensource.org/licenses/BSD-3-Clause</url>
    <distribution>repo</distribution>
  </license>
</licenses>
<developers>
  <developer>
    <id>sbeamer</id>
    <name>Scott Beamer</name>
    <email>sbeamer@ucsc.edu</email>
    <organization>UC Santa Cruz</organization>
  </developer>
</developers>
)

publishTo := {
  val v = version.value
  val nexus = "https://s01.oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT")) {
    Some("snapshots".at(nexus + "content/repositories/snapshots"))
  } else {
    Some("releases".at(nexus + "service/local/staging/deploy/maven2"))
  }
}
