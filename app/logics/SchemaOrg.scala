package logics

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.EnglishCaseConverter
import com.aha00a.commons.utils.Using
import logics.wikis.interpreters.ahaMark.AhaMarkLink
import models.ContextWikiPage
import play.api.libs.json.JsLookupResult
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

  lazy val jsonTree: JsValue = Json.parse(readResourceString("public/schema.org/26.0/tree.pruned.jsonld"))
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



  lazy val jsonAllLayers: JsValue = Json.parse(readResourceString("public/schema.org/26.0/schemaorg-current-https.jsonld"))
  lazy val seqAll:Seq[SchemaType] = {
    val values: Seq[JsValue] = (jsonAllLayers \ "graph").as[Seq[JsValue]]
    values.map(v =>{
      val id = (v \ "id").as[String]
      val typeStr: String = getSeqString(v \ "type").find(v => v == "Class" || v == "Property").getOrElse("")
      val subClassOf: Seq[String] = getSeqString(v \ "subClassOf")
      val domainIncludes: Seq[String] = getSeqString(v \ "domainIncludes")
      val comment = (v \ "comment" \ "value").asOpt[String].getOrElse((v \ "comment").as[String])
      val supersededBy: Seq[String] = getSeqString(v \ "supersededBy")
      SchemaType(id, typeStr, subClassOf, domainIncludes, comment, supersededBy)
    })
  }
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
