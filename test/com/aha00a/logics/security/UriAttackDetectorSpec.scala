package com.aha00a.logics.security

import logics.security.UriAttackDetector
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UriAttackDetectorSpec extends AnyFlatSpec with Matchers {
  "UriAttackDetector" should "detect known wordpress probing path" in {
    UriAttackDetector.isAttack("/wp-login.php") shouldBe true
  }

  it should "detect random php shell probing path" in {
    UriAttackDetector.isAttack("/wgift1.php") shouldBe true
    UriAttackDetector.isAttack("/x=34.php") shouldBe true
  }

  it should "not block regular wiki page path" in {
    UriAttackDetector.isAttack("/w/%EA%B9%80%ED%9A%A8%EC%84%9C") shouldBe false
    UriAttackDetector.isAttack("/public/main.css") shouldBe false
  }
}
