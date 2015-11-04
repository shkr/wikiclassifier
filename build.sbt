name := "wikiclassifier"

version := "1.0"

scalaVersion := "2.11.7"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.4-M3",
  "com.typesafe.akka" % "akka-stream-experimental_2.11" % "1.0",
  "org.scalanlp" %% "epic" % "0.3.1",
  "com.typesafe" % "config" % "1.3.0"
)
    