name := """AhaWiki"""
organization := "com.aha00a"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.13.2"
scalacOptions ++= Seq("-feature", "-language:implicitConversions", "-language:reflectiveCalls", "-language:postfixOps")

libraryDependencies += guice
libraryDependencies += jdbc
libraryDependencies += ehcache
libraryDependencies += cacheApi
libraryDependencies += ws
libraryDependencies += specs2 % Test
libraryDependencies += filters
libraryDependencies += evolutions
libraryDependencies += "org.playframework.anorm" % "anorm_2.13" % "2.6.5"
libraryDependencies += "com.h2database" % "h2" % "1.4.189"
libraryDependencies += "mysql" % "mysql-connector-java" % "5.1.18"
libraryDependencies += "net.sf.supercsv" % "super-csv" % "2.3.1"
libraryDependencies += "com.github.rjeschke" % "txtmark" % "0.13"
libraryDependencies += "com.google.oauth-client" % "google-oauth-client" % "1.20.0"
libraryDependencies += "io.github.java-diff-utils" % "java-diff-utils" % "4.5"
libraryDependencies += "org.jsoup" % "jsoup" % "1.13.1"
//libraryDependencies += "com.twitter.penguin" % "korean-text" % "4.1.2"
//libraryDependencies += "org.bitbucket.eunjeon" %% "seunjeon" % "1.3.1"
libraryDependencies += "org.scala-lang.modules" %% "scala-async" % "0.10.0"
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided
libraryDependencies += "org.parboiled" %% "parboiled" % "2.2.0"
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "5.1.0" % Test

// Adds additional packages into Twirl
//TwirlKeys.templateImports += "com.example.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "com.example.binders._"

includeFilter in (Assets, LessKeys.less) := "*.less"
excludeFilter in (Assets, LessKeys.less) := "_*.less"