package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import models.PageContent

object PageContentUnit {
  def run(testUtil: TestUtil): Unit = {
    import testUtil.assertEquals

    val pageContent1 = PageContent("#!read all\n#!write aha00a\n#!redirect FrontPage")
    assertEquals(pageContent1.redirect, Some("FrontPage"))
    assertEquals(pageContent1.shebang, List())

    val pageContent2 = PageContent("#!Wiki\n#!Html\nsomething")
    assertEquals(pageContent2.shebang, List("Wiki", "Html"))
    assertEquals(pageContent2.content, "something")

    val pageContent3 = PageContent("#!WikiSyntaxPreview Vim\n#!java\nclass C {\n    private String s = \"Hello === java!\";\n}\n")
    assertEquals(pageContent3.shebang, List("WikiSyntaxPreview", "Vim", "java"))

    val pageContent4 = PageContent("aaa\n[[[#!Vim java\na\n\nb\n\nc\n]]]\n")
    assertEquals(pageContent4.content, "aaa\n[[[#!Vim java\na\n\nb\n\nc\n]]]")

    val str = "str"
    assertEquals(PageContent.unapply(PageContent(str)), Some(str))
  }
}
