name := """AhaWiki"""
organization := "com.aha00a"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.13.16"
scalacOptions ++= Seq("-feature", "-language:implicitConversions", "-language:reflectiveCalls", "-language:postfixOps", "-release:11")
javacOptions ++= Seq("--release", "11")

libraryDependencies += guice
libraryDependencies += jdbc
//libraryDependencies += ehcache
libraryDependencies += cacheApi
libraryDependencies += ws
libraryDependencies += specs2 % Test
libraryDependencies += filters
libraryDependencies += evolutions
libraryDependencies += "org.playframework.anorm" %% "anorm" % "2.7.0"
libraryDependencies += "com.h2database" % "h2" % "2.3.232"
libraryDependencies += "com.mysql" % "mysql-connector-j" % "8.0.33"
libraryDependencies += "net.sf.supercsv" % "super-csv" % "2.3.1"
libraryDependencies += "com.github.rjeschke" % "txtmark" % "0.13"
libraryDependencies += "io.github.java-diff-utils" % "java-diff-utils" % "4.15"
libraryDependencies += "org.jsoup" % "jsoup" % "1.19.1"
//libraryDependencies += "com.twitter.penguin" % "korean-text" % "4.1.2"
//libraryDependencies += "org.bitbucket.eunjeon" %% "seunjeon" % "1.3.1"
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "7.0.2" % Test
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.3"
// S3 only. The `aws-java-sdk` bundle depends on every AWS service and was most of what `stage`
// shipped; another service means adding its own `aws-java-sdk-<service>` module next to this one.
libraryDependencies += "com.amazonaws" % "aws-java-sdk-s3" % "1.12.288"
// The SDK's HTTP client, at the version it ran on in production. The SDK asks for httpclient
// 4.5.13 (and so httpcore 4.4.13); what lifted both was google-oauth-client, which nothing used and
// is gone. S3 is the one path the specs cannot exercise, so it keeps the client it has been calling.
dependencyOverrides += "org.apache.httpcomponents" % "httpclient" % "4.5.14"
dependencyOverrides += "org.apache.httpcomponents" % "httpcore" % "4.4.16"
libraryDependencies += "com.github.karelcemus" %% "play-redis" % "5.4.0"
// Redis pub/sub for cross-instance page.updated (CrossInstanceBus). play-redis is the cache and
// does not expose SUBSCRIBE/PUBLISH, so a dedicated client is used on the same shared Redis.
libraryDependencies += "redis.clients" % "jedis" % "5.2.0"
libraryDependencies += "dev.zio" %% "zio-json" % "0.7.3" // DON'T update. Class File version matter

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % "0.12.3")

Compile / doc / sources := Seq.empty
Test / doc / sources := Seq.empty


// Adds additional packages into Twirl
//TwirlKeys.templateImports += "com.example.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "com.example.binders._"

//includeFilter in (Assets, LessKeys.less) := "*.less"
//excludeFilter in (Assets, LessKeys.less) := "_*.less"

// The packaged conf/ carries only what the repository tracks.
//
// `sbt stage` copies every file under conf/, and gitignore has no say in that — it governs git.
// Local and per-deployment configs kept there therefore rode along into every release: a dev
// database password and a `play.http.secret.key` landed on the production server, world readable,
// in a file the app never even opens (it is started with an absolute `-Dconfig.file`).
//
// Everyone already believes "an ignored file does not ship". This makes that true for conf/ by
// asking git what it tracks, rather than guessing from filenames — a name-based rule only catches
// the shapes someone thought of, and the file that started this had none of them.
//
// Without git, it falls back to those shapes and says so. Quietly shipping the lot is what this
// exists to prevent; quietly shipping nothing would be worse.
Universal / mappings := {
  import scala.sys.process._
  val log = streams.value.log
  val root = baseDirectory.value
  val tracked: Option[Set[String]] =
    try Some(Process(Seq("git", "ls-files", "conf"), root).!!(ProcessLogger(_ => (), _ => ()))
      .linesIterator.map(_.trim.replace(java.io.File.separatorChar, '/')).filter(_.nonEmpty).toSet)
    catch { case _: Throwable => None }

  val looksLocal = (name: String) =>
    name.contains(".local.") || name.endsWith(".bak") || name.contains(".bak-") ||
      name.endsWith(".orig") || name.endsWith("~")

  if (tracked.isEmpty)
    log.warn("conf/: git unavailable, falling back to name patterns. Check the release for local configs.")

  val (kept, dropped) = (Universal / mappings).value.partition { case (_, path) =>
    val p = path.replace(java.io.File.separatorChar, '/')
    if (!p.startsWith("conf/")) true
    else tracked match {
      case Some(files) => files.contains(p)
      case None => !looksLocal(p.split('/').last)
    }
  }
  dropped.foreach { case (_, path) => log.info(s"conf/: not packaged (untracked): $path") }

  // In fallback the warning above asks whoever is deploying to check the release, so list what
  // there is to check. Naming only what was dropped is no help here: the danger is a file the
  // patterns did not recognise, which by definition is not in that list. Exercised 2026-08-15 by
  // building with git off the PATH — a `*.local.*` bait was dropped and a hostname-named one
  // shipped, which is the documented limit of the fallback rather than a fault in it.
  // Only the top level of conf/, because that is where a stray config lands and the whole list —
  // every default page, every evolution — is long enough to hide one in.
  if (tracked.isEmpty)
    kept.map(_._2.replace(java.io.File.separatorChar, '/'))
      .filter(p => p.startsWith("conf/") && !p.stripPrefix("conf/").contains('/')).sorted
      .foreach(p => log.warn(s"conf/: packaged on the name rule alone: $p"))

  kept
}
