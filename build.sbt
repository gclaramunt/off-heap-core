name := "off-heap-core"

val commonSettings = Seq(
  organization := "off-heap-core",
  version := "0.1-SNAPSHOT",
  scalaVersion := "2.11.7",
  scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-feature",
    "-Yinfer-argument-types",
    "-Ybackend:GenBCode",
    "-Ydelambdafy:method",
    "-target:jvm-1.8"
  ),
  fork := true,
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
)

lazy val root = project in file(".")

lazy val jmh = project.settings(commonSettings:_*).dependsOn(root).enablePlugins(JmhPlugin)

commonSettings

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scala-lang.modules" %% "scala-java8-compat" % "0.5.0",
  "com.chuusai" %% "shapeless" % "2.2.5"
)

initialCommands in Compile in console := "import ohc._"
