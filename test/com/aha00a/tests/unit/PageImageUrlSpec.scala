package com.aha00a.tests.unit

import logics.wikis.PageImageUrl
import org.scalatestplus.play.PlaySpec

class PageImageUrlSpec extends PlaySpec {
  private val host = "example.test"

  "absolute" should {
    "keep a full URL and complete the rest with the one slash it needs" in {
      PageImageUrl.absolute("https://a.test/x.png", "https", host) mustBe "https://a.test/x.png"
      PageImageUrl.absolute("//a.test/x.png", "https", host) mustBe "https://a.test/x.png"
      PageImageUrl.absolute("/public/x.png", "https", host) mustBe "https://example.test/public/x.png"
      PageImageUrl.absolute("public/x.png", "https", host) mustBe "https://example.test/public/x.png"
    }
  }

  "forSocialPreview" should {
    "give an address that stays valid" in {
      PageImageUrl.forSocialPreview(" /public/x.png ", host) mustBe Some("https://example.test/public/x.png")
    }

    "refuse what a preview cannot show or should not carry" in {
      PageImageUrl.forSocialPreview("attachment:Page/x.png", host) mustBe None
      PageImageUrl.forSocialPreview("/public/x.SVG?v=1", host) mustBe None
      PageImageUrl.forSocialPreview("  ", host) mustBe None
      PageImageUrl.forSocialPreview(null, host) mustBe None
    }
  }
}
