package logics

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.EnglishCaseConverter
import com.aha00a.commons.utils.Using
import logics.wikis.interpreters.ahaMark.AhaMarkLink
import models.ContextWikiPage
import play.api.libs.json.JsLookupResult
import play.api.libs.json.JsObject
import play.api.libs.json.JsValue
import play.api.libs.json.Json

import scala.Ordering.Implicits._
import scala.collection.mutable
import scala.io.Codec
import scala.xml.Elem
import scala.xml.NodeSeq

object CalculatedSchemaOrg {
  case class SchemaType(
                         id:String,
                         schemaType:String,
                         subClassOf: Seq[String],
                         domainIncludes: Seq[String],
                         comment: String,
                         supersededBy: Seq[String]
                       ) {
    def toXmlSpan(toTitleCase: Boolean = true, classes: Seq[String] = Seq()): Elem = {
      val (title, seqClass) = if (supersededBy.nonEmpty) (
        "Superseded by " + supersededBy.mkString(",") + "\n" + comment,
        classes :+ "supersededBy"
      ) else (
        comment,
        classes
      )

      <a href={"/w/schema:" + id} title={title} class={seqClass.mkString(" ")}>{if(toTitleCase) EnglishCaseConverter.camelCase2TitleCase(id) else id}</a>
    }
    def toAhaMarkLink(implicit wikiContext:ContextWikiPage): AhaMarkLink = {
      val title = if (id.matches("^[A-Za-z0-9]+$")) EnglishCaseConverter.pascalCase2TitleCase(id) else id
      AhaMarkLink(s"schema:$id", title)
    }
  }

  def withNameSpace(s: String): String = s"schema:$s"

  // 클래스패스에서 읽는다. 파일시스템 경로로 읽으면 sbt stage/dist 배포본에서 동작하지 않는다
  // (작업 디렉터리가 소스 체크아웃일 때만 우연히 동작한다).
  // public/** 는 sbt-web 이 assets JAR 안 public/ 으로 그대로 패키징하므로 경로가 동일하다.
  private def readResourceString(path: String): String = {
    val is = getClass.getClassLoader.getResourceAsStream(path)
    require(is != null, s"resource not found on classpath: $path")
    Using(scala.io.Source.fromInputStream(is)(Codec.UTF8))(_.mkString)
  }

  // The one version the application reads. public/schema.org/ also holds 5.0 and 14.0, which are
  // history. These files are not what schema.org publishes — see docs/ahawiki.net/'Dev
  // SchemaOrgVocabulary' for what produces them and how to raise this.
  private val version = "30.1"

  /**
   * Classes this wiki needs that schema.org does not define, in the same shape as the vocabulary.
   *
   * Without this a class the vocabulary does not know still renders — `getSchemaClass` falls back
   * to an empty SchemaType — but it has no parent, no description and no inherited properties, and
   * `renderExistingPages` drops its pages into a flat "Custom" heap at the bottom. Naming a parent
   * is what puts them back in the tree.
   */
  lazy val jsonCustom: JsValue = Json.parse(readResourceString("public/schema.org/custom.jsonld"))

  /**
   * The class tree, with each custom class grafted under the parent it declares.
   *
   * The tree is a separate file from the vocabulary and knows nothing about custom classes, so
   * merging only into `seqAll` would give `Standard` a parent everywhere except the one place that
   * lists pages by class.
   */
  lazy val jsonTree: JsValue = {
    val base = Json.parse(readResourceString(s"public/schema.org/$version/tree.pruned.jsonld"))
    seqCustom.filter(_.schemaType == "Class").foldLeft(base) { (tree, custom) =>
      custom.subClassOf.headOption.fold(tree)(parent => graftChild(tree, parent, custom.id))
    }
  }

  /** `tree` with `{"id": child}` appended to the children of the node named `parent`. */
  private def graftChild(tree: JsValue, parent: String, child: String): JsValue = tree match {
    case node: JsObject =>
      val children = (node \ "children").asOpt[Seq[JsValue]].getOrElse(Seq())
      if ((node \ "id").asOpt[String].contains(parent))
        node ++ Json.obj("children" -> (children :+ Json.obj("id" -> child)))
      else if (children.isEmpty) node
      else node ++ Json.obj("children" -> children.map(graftChild(_, parent, child)))
    case other => other
  }

  def getHtmlTree(q:String, node:JsValue = jsonTree): NodeSeq = {
    val id = (node \ "id").as[String]
    val idWithNameSpace = withNameSpace(id)
    val children = (node \ "children").asOpt[Seq[JsValue]]
    if(id.containsIgnoreCase(q)) {
      <ul>
        <li><a href={s"/w/$idWithNameSpace"}>{id}</a></li>
        {children.map(seq => seq.map(n => getHtmlTree("", n))).getOrElse(NodeSeq.Empty)}
      </ul>
    } else {
      val c: Seq[xml.Node] = children.map(seq => seq.map(n => getHtmlTree(q, n))).getOrElse(NodeSeq.Empty).flatten
      if(c.isEmpty) {
        c
      } else {
        <ul>
          <li><a href={s"/w/$idWithNameSpace"}>{id}</a></li>
          {c}
        </ul>
      }
    }
  }

  def renderExistingPages(map: Map[String, Seq[String]]): String = {

    val defined = renderSchemaClassTreeWithExistingPages(map, jsonTree)
    val mapCustom = map.filter(e => !mapClass.isDefinedAt(e._1))
    if(mapCustom.isEmpty) {
      defined
    } else {
      s"""
         |$defined
         |
         |= Custom
         |${mapCustom.toSeq.sortBy(_._1).map(k =>
      s"""== ["schema:${k._1}" ${EnglishCaseConverter.pascalCase2TitleCase(k._1)}] (${k._2.distinct.size}) == #${schemaHeadingId(k._1)}
         |${renderPageListColumns(k._2)}
         |""".stripMargin).mkString(System.lineSeparator)}
         |""".stripMargin
    }

  }

  def renderSchemaClassTreeWithExistingPages(map: Map[String, Seq[String]], node:JsValue = jsonTree, depth: Int = 1): String = {
    renderSchemaClassTreeWithExistingPagesAndPages(map, node, depth)._1
  }

  private def renderSchemaClassTreeWithExistingPagesAndPages(map: Map[String, Seq[String]], node:JsValue = jsonTree, depth: Int = 1): (String, Set[String]) = {
    val id = (node \ "id").as[String]
    val children = (node \ "children").asOpt[Seq[JsValue]].getOrElse(Seq())

    val seqNodeSeqWithPages: Seq[(String, Set[String])] = children.map(j => renderSchemaClassTreeWithExistingPagesAndPages(map, j, depth + 1)).filter(_._1.nonEmpty)
    val seqNodeSeq: Seq[String] = seqNodeSeqWithPages.map(_._1)
    val seq = map.getOrElse(id, Seq())
    val pages = seq.toSet ++ seqNodeSeqWithPages.flatMap(_._2)
    if(pages.isEmpty) {
      ("", Set())
    } else {
      (s"""${"=" * depth} ["schema:$id" ${EnglishCaseConverter.pascalCase2TitleCase(id)}] (${pages.size}) ${"=" * depth} #${schemaHeadingId(id)}
         |${renderPageListColumns(seq)}
         |${seqNodeSeq.mkString(System.lineSeparator)}
         |""".stripMargin, pages)
    }
  }

  private def renderPageListColumns(seq: Seq[String]): String = {
    if(seq.isEmpty) {
      ""
    } else {
      s"""<Columns count="3" gap="16" minWidth="220">
         |${seq.map(s => s""" 1. ["$s"]""").mkString(System.lineSeparator)}
         |</Columns>""".stripMargin
    }
  }

  private def schemaHeadingId(id: String): String = id.replaceAll("""\s+""", "-").replaceAll("""[#.]+""", "-")



  lazy val jsonAllLayers: JsValue = Json.parse(readResourceString(s"public/schema.org/$version/schemaorg-current-https.jsonld"))

  /**
   * Terms this vocabulary carries that are not schema.org's, and must not reach the class browser
   * or the property suggestions. Both filters are no-ops on 26.0 — they leave its counts exactly
   * as they were — and both bite from 27.0 onwards.
   *
   *  - A namespaced id. From 27.0 the release bundles other vocabularies: `bibo:`, `cmns-*:`,
   *    `fibo-*:`, `gs1:`, `unece:`, `eli:`. 30.1 brings 231 of them, every one typed Class or
   *    Property, so without this they land in mapClass and mapProperty as if they were ours.
   *
   *  - No comment. SchemaOrgTransform strips the `rdf:` and `rdfs:` prefixes along with
   *    schema.org's own, which it has to — `rdfs:Class` is how a class says it is one. The side
   *    effect is that `rdf:type` and `rdfs:label` arrive as bare `type` and `label`, looking like
   *    schema.org properties and colliding with the keys of the same name. Every real term carries
   *    rdfs:comment and these two carry nothing, which is the difference worth reading.
   */
  private def isSchemaOrgTerm(id: String, comment: Option[String]): Boolean =
    !id.contains(":") && comment.nonEmpty

  private def parseGraph(json: JsValue): Seq[SchemaType] = {
    val values: Seq[JsValue] = (json \ "graph").as[Seq[JsValue]]
    values.flatMap(v =>{
      val id = (v \ "id").as[String]
      // Both spellings occur: a plain string, or {language, value} once the term is localised.
      val comment: Option[String] = (v \ "comment" \ "value").asOpt[String].orElse((v \ "comment").asOpt[String])
      Option.when(isSchemaOrgTerm(id, comment)) {
        val typeStr: String = getSeqString(v \ "type").find(v => v == "Class" || v == "Property").getOrElse("")
        val supersededBy: Seq[String] = getSeqString(v \ "supersededBy")
        SchemaType(id, typeStr, keptOnly(v \ "subClassOf"), keptOnly(v \ "domainIncludes"), comment.get, supersededBy)
      }
    })
  }

  private lazy val seqSchemaOrg: Seq[SchemaType] = parseGraph(jsonAllLayers)

  /**
   * Our own terms, minus any that schema.org has since defined.
   *
   * The moment schema.org defines a name we had been filling in, the real one wins and ours stops
   * being used — otherwise a stand-in would quietly outlive its reason. A test names the collision
   * rather than leaving it to be noticed: that is when to delete our entry and, if it was ever
   * proposed, close the proposal.
   *
   * The maps and the tree both read this one list, so a dropped term cannot linger in one of them.
   */
  lazy val seqCustom: Seq[SchemaType] = {
    val taken = seqSchemaOrg.map(_.id).toSet
    parseGraph(jsonCustom).filterNot(custom => taken.contains(custom.id))
  }

  lazy val seqAll:Seq[SchemaType] = seqSchemaOrg ++ seqCustom

  /**
   * References to terms the filter above dropped, removed.
   *
   * Ten schema.org classes name a bundled foreign class as a parent — `Brand` is a
   * `cmns-cls:Classifier`, `Country` a `cmns-ge:GeopoliticalEntity`. Dropping the term and
   * keeping the reference leaves `getParents` handing out a name that is in no map, which
   * `getPathHierarchy` then walks into. Every one of the ten also has a schema.org parent, so
   * removing the foreign half orphans nothing.
   *
   * domainIncludes has no such reference today. The same rule covers it so that a later release
   * cannot introduce one quietly.
   */
  private def keptOnly(lookup: JsLookupResult): Seq[String] = getSeqString(lookup).filterNot(_.contains(":"))
  lazy val seqClass: Seq[SchemaType] = seqAll.filter(_.schemaType == "Class")
  lazy val seqProperty: Seq[SchemaType] = seqAll.filter(_.schemaType == "Property")
  lazy val mapAll: Map[String, SchemaType] = seqAll.map(n => (n.id, n)).toMap
  lazy val mapClass: Map[String, SchemaType] = seqClass.map(n => (n.id, n)).toMap
  lazy val mapProperty: Map[String, SchemaType] = seqProperty.map(n => (n.id, n)).toMap

  def getHtmlProperties(schema:String, seqPropertyUsed:Seq[String]): Elem = {
    val seqClass = getClassHierarchy(schema)
    <div>
      {
        seqClass.map(c => {
          <div class="properties">
            <h6>Properties of {c}</h6>
            <div>
              {
                val groupByFirstLetter: Map[Char, Seq[SchemaType]] = seqProperty.filter(p => p.domainIncludes.contains(c)).sortBy(_.id).groupBy(p => p.id(0))
                groupByFirstLetter.keys.toSeq.sorted.map { firstLetter =>
                  <ol class="groupedProperties">
                    {
                      groupByFirstLetter(firstLetter)
                        .map(p => <li>{p.toXmlSpan(toTitleCase = false, if(seqPropertyUsed.contains(p.id)){Seq("match")}else{Seq("")})}</li>)
                    }
                  </ol>
                }
              }
            </div>
          </div>
        })
      }
    </div>
  }

  def getSchemaClass(schema:String): SchemaType = mapClass.getOrElse(schema, SchemaType(schema, "Class", Seq(), Seq(), "", Seq()))
  def getClassHierarchy(schema: String): Seq[String] = {
    mapClass.get(schema).map(v => v.id +: v.subClassOf.flatMap(p => getClassHierarchy(p))).getOrElse(Seq())
  }

  def getParents(schema:String): Seq[String] = {
    mapClass.get(schema) match {
      case Some(n) => n.subClassOf
      case None => Seq()
    }
  }

  def traverse(path: Seq[String], callback: Seq[String] => Unit): Unit = {
    val strings = getParents(path.head)
    if (strings.isEmpty) {
      callback(path)
    } else {
      strings.foreach(p => traverse(p +: path, callback))
    }
  }

  def getPathHierarchy(schema:String): Seq[Seq[String]] = {
    val buffer: mutable.Buffer[Seq[String]] = mutable.Buffer[Seq[String]]()
    traverse(Seq(schema), s => buffer += s)
    buffer.sorted.toSeq
  }

  def getSeqString(jsLookupResult: JsLookupResult): Seq[String] = {
    None
      .orElse(jsLookupResult.asOpt[String].map(v => Seq(v)))
      .orElse(jsLookupResult.asOpt[Seq[String]])
      .getOrElse(Seq[String]())
  }

}
