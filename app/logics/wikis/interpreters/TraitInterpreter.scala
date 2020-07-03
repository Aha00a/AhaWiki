package logics.wikis.interpreters

import models.{SchemaOrg, WikiContext}

trait TraitInterpreter {

  import models.tables.Link

  val name: String = getClass.getSimpleName.replaceAll("^Interpreter", "").replaceAll("""\$$""", "")

  def toHtmlString(content: String)(implicit wikiContext: WikiContext): String

  def toSeqWord(content: String)(implicit wikiContext: WikiContext): Seq[String] = content.split("""\s+""")

  def toSeqLink(content: String)(implicit wikiContext: WikiContext): Seq[Link]

  def toSeqSchemaOrg(content: String)(implicit wikiContext: WikiContext): Seq[SchemaOrg] = Seq()
}

