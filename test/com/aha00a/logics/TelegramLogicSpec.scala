package com.aha00a.logics

import logics.TelegramLogic
import org.scalatest.freespec.AnyFreeSpec

import java.net.ConnectException
import scala.util.Failure
import scala.util.Success

class TelegramLogicSpec extends AnyFreeSpec {
  "sendOutcomeWarning" - {
    "is quiet when Telegram answered 200" in {
      assert(TelegramLogic.sendOutcomeWarning("-100", Success((200, "{}"))) === None)
    }

    "names the status and the first 200 characters of the body otherwise" in {
      val warning = TelegramLogic.sendOutcomeWarning("-100", Success((401, "x" * 500))).get
      assert(warning.contains("status=401"))
      assert(warning.contains("x" * 200) && !warning.contains("x" * 201))
    }

    // Until 2026-09-10 a send that never reached Telegram logged nothing at all.
    "reports a transport failure by class name, and leaves the message out" in {
      val failure = new ConnectException("https://api.telegram.org/bot123:SECRET/sendMessage")
      val warning = TelegramLogic.sendOutcomeWarning("-100", Failure(failure)).get
      assert(warning.contains("java.net.ConnectException"))
      assert(!warning.contains("SECRET"), "the request URL carries the bot token")
    }
  }

  "takeBatch" - {
    "joins what fits under the limit, oldest first, and leaves the rest" in {
      val (text, rest) = TelegramLogic.takeBatch(Vector("aaaa", "bbbb", "cccc"), limit = 10)
      assert(text === "aaaa\n\nbbbb")
      assert(rest === Vector("cccc"))
    }

    "still takes one message that is longer than the limit on its own" in {
      val (text, rest) = TelegramLogic.takeBatch(Vector("x" * 20, "y"), limit = 10)
      assert(text === "x" * 20)
      assert(rest === Vector("y"))
    }
  }

  "retryAfterSeconds" - {
    "reads the wait out of a 429" in {
      val body = """{"ok":false,"error_code":429,"description":"Too Many Requests: retry after 5","parameters":{"retry_after":5}}"""
      assert(TelegramLogic.retryAfterSeconds(429, body) === Some(5))
    }

    "is None for anything else" in {
      assert(TelegramLogic.retryAfterSeconds(400, """{"parameters":{"retry_after":5}}""") === None)
      assert(TelegramLogic.retryAfterSeconds(429, "not json") === None)
    }
  }

  "TelegramChatQueue" - {
    import logics.TelegramChatQueue
    import scala.collection.mutable
    import scala.concurrent.ExecutionContext
    import scala.concurrent.Future
    import scala.concurrent.duration._

    // Runs every callback on the calling thread, so the queue's steps happen in order inside the test.
    implicit val sameThread: ExecutionContext = new ExecutionContext {
      def execute(runnable: Runnable): Unit = runnable.run()
      def reportFailure(cause: Throwable): Unit = throw cause
    }

    class Harness(answers: (Int, String)*) {
      val posted = mutable.Buffer.empty[String]
      val warnings = mutable.Buffer.empty[String]
      val timers = mutable.Queue.empty[(FiniteDuration, () => Unit)]
      private val replies = mutable.Queue(answers: _*)
      val queue = new TelegramChatQueue(
        "-100",
        text => { posted += text; Future.successful(if (replies.nonEmpty) replies.dequeue() else (200, "{}")) },
        (delay, run) => timers.enqueue((delay, run)),
        warnings += _,
      )
      def tick(): FiniteDuration = { val (delay, run) = timers.dequeue(); run(); delay }
    }

    "sends the first message at once and joins what arrives meanwhile into the next one" in {
      val h = new Harness()
      h.queue.enqueue("one")
      h.queue.enqueue("two")
      h.queue.enqueue("three")
      assert(h.posted === Seq("one"))
      assert(h.tick() === 3.seconds)
      assert(h.posted === Seq("one", "two\n\nthree"))
    }

    // The 2026-11-18 burst on sst: 180 notifications refused with 429 and dropped.
    "waits as long as a 429 says and sends the same text again" in {
      val tooMany = (429, """{"ok":false,"error_code":429,"parameters":{"retry_after":5}}""")
      val h = new Harness(tooMany)
      h.queue.enqueue("one")
      assert(h.tick() === 6.seconds)
      assert(h.posted === Seq("one", "one"))
      assert(h.warnings.isEmpty)
    }

    "gives up on a text after five refusals and says so" in {
      val tooMany = (429, """{"parameters":{"retry_after":1}}""")
      val h = new Harness(Seq.fill(6)(tooMany): _*)
      h.queue.enqueue("one")
      (1 to 5).foreach(_ => h.tick())
      assert(h.posted.size === 6)
      assert(h.warnings.size === 1 && h.warnings.head.contains("status=429"))
      h.tick()
      h.queue.enqueue("two")
      assert(h.posted.last === "two")
    }
  }
}
