lazy val buildSettings = Seq(
  organization := "com.github.finagle",
  scalaVersion := "2.11.7",
  crossScalaVersions := Seq("2.10.6", "2.11.7")
)

lazy val compilerOptions = Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yinline-warnings",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture"
)

lazy val testDependencies = Seq(
  "org.scalatest"   %%  "scalatest" % "2.2.6"
)


lazy val baseSettings = Seq(
  scalacOptions ++= compilerOptions, 
  scalacOptions in (Compile, console) := compilerOptions,
  libraryDependencies ++= testDependencies.map(_ % "test"),
  resolvers += Resolver.sonatypeRepo("snapshots"),
  resolvers += "Twitter Maven repo" at "http://maven.twttr.com/"
)

lazy val allSettings = buildSettings ++ baseSettings 

lazy val coreVersion = "0.1.1"

lazy val catsVersion = "0.4.1"

lazy val finagleVersion = "6.33.0"

lazy val nettyVersion = "4.1.0.CR2"

lazy val roc = project.in(file("."))
  .settings(moduleName := "root")
  .settings(allSettings)
  .settings(noPublishSettings)
  .aggregate(core)
  .dependsOn(core)

lazy val core =  project
  .settings(moduleName := "roc-core")
  .settings(version := coreVersion)
  .settings(allSettings:_*)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel"   %%  "cats"          %  catsVersion,
      "io.netty"        %   "netty-buffer"  %  nettyVersion,
      "com.twitter"     %%  "finagle-core"  %  finagleVersion
    )
  )

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)
