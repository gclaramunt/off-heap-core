name := "off-heap-core"

organization := "off-heap-core"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.7"

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
  "-Yinfer-argument-types",
  "-Ybackend:GenBCode",
  "-Ydelambdafy:method",
  "-target:jvm-1.8"
)

fork := true

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-java8-compat" % "0.5.0",
  "com.chuusai" %% "shapeless" % "2.2.5"
)
