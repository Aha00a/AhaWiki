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
}
