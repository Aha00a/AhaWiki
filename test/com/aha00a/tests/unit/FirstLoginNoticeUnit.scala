package com.aha00a.tests.unit

import logics.FirstLoginNotice
import org.scalatestplus.play.PlaySpec

class FirstLoginNoticeUnit extends PlaySpec {
  "the first-login notice" should {
    "name the person and point at the nickname section of the settings page" in {
      val html = FirstLoginNotice.html("new_comer", "/account")
      html must include("<strong>new_comer</strong>")
      html must include("""<a href="/account#nickname">""")
    }

    // The page writes a flash value out as HTML. No nickname `User.insert` or
    // `UserNicknamePolicy` lets through holds markup today; this is what keeps the notice from
    // depending on that staying true.
    "carry the name as text, never as markup" in {
      val html = FirstLoginNotice.html("""<img src=x onerror="alert(1)">&""", "/account")
      html must include("&lt;img src=x onerror=&quot;alert(1)&quot;&gt;&amp;")
      html must not include "<img"
    }
  }
}
