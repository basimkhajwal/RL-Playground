import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val client = (project in file("client")).settings(commonSettings).settings(
  scalaJSUseMainModuleInitializer := true,
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.9.3",
    "com.thoughtworks.binding" %%% "dom" % "10.0.3",
    "com.lihaoyi" %%% "upickle" % "0.7.5"
  ),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
).enablePlugins(ScalaJSPlugin).
  dependsOn(sharedJs)


lazy val shared = (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file("shared")).settings(commonSettings).settings(
  libraryDependencies ++= Seq(
    "com.lihaoyi" %%% "upickle" % "0.7.5",
    "org.scalactic" %% "scalactic" % "3.0.8" % "test",
    "org.scalatest" %% "scalatest" % "3.0.8" % "test",
    "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
    "com.github.wookietreiber" %% "scala-chart" % "latest.integration"
  )
)
lazy val sharedJvm = shared.jvm
lazy val sharedJs = shared.js

lazy val commonSettings = Seq(
  scalaVersion := "2.12.2",
  organization := "basimkhajwal"
)
