name := """AhaWiki"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.6"

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
  "com.googlecode.java-diff-utils" % "diffutils" % "1.3.0",
  "com.twitter.penguin" % "korean-text" % "4.1.2"
)

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

// Play provides two styles of routers, one expects its actions to be injected, the
// other, legacy style, accesses its actions statically.
routesGenerator := InjectedRoutesGenerator
includeFilter in (Assets, LessKeys.less) := "*.less"
