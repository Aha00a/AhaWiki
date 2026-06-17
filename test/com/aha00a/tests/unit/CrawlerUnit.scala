package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import logics.Crawler

object CrawlerUnit {
  def run(testUtil: TestUtil): Unit = {
    import testUtil.assertEquals

    {
      val crawler = Crawler.fromHtml(
        """<html>
          |<head>
          |  <title>title</title>
          |</head>
          |<body>body</body>
          |</html>
        """.stripMargin)

      assertEquals(crawler.title, "title")
      assertEquals(crawler.description, "body")
      assertEquals(crawler.image, "")
    }

    {
      val crawler = Crawler.fromHtml(
        """<html>
          |<head>
          |  <title>title</title>
          |  <meta property="og:title" content="ogTitle">
          |  <meta property="og:description" content="ogDescription">
          |  <meta property="og:image" content="ogImage">
          |</head>
          |<body>body</body>
          |</html>
        """.stripMargin)

      assertEquals(crawler.title, "ogTitle")
      assertEquals(crawler.description, "ogDescription")
      assertEquals(crawler.image, "ogImage")
    }
  }
}
