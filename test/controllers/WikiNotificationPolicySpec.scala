package controllers

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class WikiNotificationPolicySpec extends AnyFreeSpec with Matchers {
  "Wiki Telegram notification policy" - {
    "allows normal web edits" in {
      Wiki.shouldNotifyTelegramForPageSave(isMinorEdit = false, viaApi = false) mustBe true
    }

    "skips minor edits" in {
      Wiki.shouldNotifyTelegramForPageSave(isMinorEdit = true, viaApi = false) mustBe false
    }

    "skips API edits" in {
      Wiki.shouldNotifyTelegramForPageSave(isMinorEdit = false, viaApi = true) mustBe false
    }
  }
}
