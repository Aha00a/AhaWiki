name := """AhaWiki"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.8"
scalacOptions ++= Seq("-feature", "-language:implicitConversions", "-language:reflectiveCalls", "-language:postfixOps")

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  specs2 % Test,
  filters,
  evolutions,
  "com.typesafe.play" %% "anorm" % "2.4.0",
  "com.h2database" % "h2" % "1.4.189",
  "mysql" % "mysql-connector-java" % "5.1.18",
  "net.sf.supercsv" % "super-csv" % "2.3.1",
  "com.github.rjeschke" % "txtmark" % "0.13",
  "com.google.oauth-client" % "google-oauth-client" % "1.20.0",
  "io.github.java-diff-utils" % "java-diff-utils" % "4.5"
//  "com.twitter.penguin" % "korean-text" % "4.1.2",
//  "org.bitbucket.eunjeon" %% "seunjeon" % "1.3.1"
)

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

// Play provides two styles of routers, one expects its actions to be injected, the
// other, legacy style, accesses its actions statically.
routesGenerator := InjectedRoutesGenerator
includeFilter in (Assets, LessKeys.less) := "*.less"

//<editor-fold desc="Prevent to build API document - https://www.playframework.com/documentation/2.5.x/Deploying">
sources in (Compile, doc) := Seq.empty
publishArtifact in (Compile, packageDoc) := false
//</editor-fold>

skip in update := true