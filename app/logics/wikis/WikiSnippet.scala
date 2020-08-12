package logics.wikis

object WikiSnippet {
  def notFound(name: String): String =
    s"""= $name
       |This page does not exist.
       |== Possible actions
       | * [[Html(<a href="?action=edit">create page</a>)]]
       | * Search ["https://google.com/search?q=$name" $name] on Google
       | * Search ["https://google.com/search?q=$name wiki" $name wiki] on Google
       | * Search ["https://duckduckgo.com/?q=$name" $name] on DuckDuckGo
       | * Search ["https://duckduckgo.com/?q=$name wiki" $name wiki] on DuckDuckGo
       |""".stripMargin

  def notFoundWithDayHeader(name: String): String = "[[DayHeader]]\n" + notFound(name)
}
