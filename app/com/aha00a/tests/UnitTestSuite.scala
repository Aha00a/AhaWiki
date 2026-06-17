package com.aha00a.tests

import com.aha00a.tests.unit.{BlameUnit, CrawlerUnit, HeadingNumberUnit, InterpreterBlockUnit, InterpreterMarkdownUnit, InterpreterSchemaUnit, InterpreterVimUnit, InterpreterWikiUnit, JsonUnit, MacroPeriodUnit, PageContentUnit, PermissionLogicUnit, PermissionUnit, SchemaOrgUnit, SignedReadUrlLogicUnit, TraitInterpreterUnit, UrlDetectorUnit, WikiMacrosUnit, WikiPermissionUnit}
import models.ContextWikiPage
import play.api.mvc.{AnyContent, Request}

import scala.util.control.NonFatal

object UnitTestSuite {
  case class UnitTestCase(name: String, run: () => Unit)

  case class UnitTestResult(name: String, passed: Boolean, durationMs: Long, error: Option[String])

  def cases(testUtil: TestUtil)(implicit request: Request[AnyContent], contextWikiPage: ContextWikiPage): Seq[UnitTestCase] = Seq(
    UnitTestCase("InterpreterBlockUnit", () => InterpreterBlockUnit.run(testUtil)),
    UnitTestCase("HeadingNumberUnit", () => HeadingNumberUnit.run(testUtil)),
    UnitTestCase("InterpreterVimUnit", () => InterpreterVimUnit.run(testUtil)),
    UnitTestCase("WikiMacrosUnit", () => WikiMacrosUnit.run(testUtil)),
    UnitTestCase("MacroPeriodUnit", () => MacroPeriodUnit.run(testUtil)),
    UnitTestCase("UrlDetectorUnit", () => UrlDetectorUnit.run(testUtil)),
    UnitTestCase("SchemaOrgUnit", () => SchemaOrgUnit.run(testUtil)),
    UnitTestCase("TraitInterpreterUnit", () => TraitInterpreterUnit.run(testUtil)),
    UnitTestCase("SignedReadUrlLogicUnit", () => SignedReadUrlLogicUnit.run(testUtil)),
    UnitTestCase("InterpreterMarkdownUnit", () => InterpreterMarkdownUnit.run(testUtil)),
    UnitTestCase("JsonUnit", () => JsonUnit.run(testUtil)),
    UnitTestCase("InterpreterSchemaUnit", () => InterpreterSchemaUnit.run(testUtil)),
    UnitTestCase("PermissionUnit", () => PermissionUnit.run(testUtil)),
    UnitTestCase("BlameUnit", () => BlameUnit.run(testUtil)),
    UnitTestCase("PageContentUnit", () => PageContentUnit.run(testUtil)),
    UnitTestCase("PermissionLogicUnit", () => PermissionLogicUnit.run(testUtil)),
    UnitTestCase("WikiPermissionUnit", () => WikiPermissionUnit.run(testUtil)),
    UnitTestCase("InterpreterWikiUnit", () => InterpreterWikiUnit.run(testUtil)),
    UnitTestCase("CrawlerUnit", () => CrawlerUnit.run(testUtil)),
  )

  def runAll(
    testUtil: TestUtil
  )(
    onFailure: (String, Throwable) => Unit = (_, _) => ()
  )(implicit request: Request[AnyContent], contextWikiPage: ContextWikiPage): Seq[UnitTestResult] =
    cases(testUtil).map(runCase(_, onFailure))

  private def runCase(test: UnitTestCase, onFailure: (String, Throwable) => Unit): UnitTestResult = {
    val startedAtNanos = System.nanoTime()
    try {
      test.run()
      UnitTestResult(test.name, passed = true, elapsedMillisSince(startedAtNanos), None)
    } catch {
      case NonFatal(e) =>
        onFailure(test.name, e)
        UnitTestResult(test.name, passed = false, elapsedMillisSince(startedAtNanos), Some(errorSummary(e)))
    }
  }

  private def elapsedMillisSince(startedAtNanos: Long): Long =
    (System.nanoTime() - startedAtNanos) / 1000000

  private def errorSummary(e: Throwable): String = {
    val message = Option(e.getMessage).map(_.trim).filter(_.nonEmpty)
    message.map(m => s"${e.getClass.getSimpleName}: $m").getOrElse(e.getClass.getSimpleName)
  }
}
