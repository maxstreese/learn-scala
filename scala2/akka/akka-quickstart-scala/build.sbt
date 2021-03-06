name := "akka-quickstart-scala"

version := "1.0"

scalaVersion := "2.13.5"

lazy val akkaVersion = "2.6.14"

libraryDependencies ++= Seq(
  "ch.qos.logback"     % "logback-classic"          % "1.2.3"           ,
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion % Test,
  "com.typesafe.akka" %% "akka-actor-typed"         % akkaVersion       ,
  "org.scalatest"     %% "scalatest"                % "3.1.0"     % Test
)
