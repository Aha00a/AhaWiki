package logics

import java.io.File

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.{EnglishCaseConverter, Using}
import logics.wikis.interpreters.InterpreterWiki.LinkMarkup
import models.WikiContext
import play.api.libs.json.{JsLookupResult, JsValue, Json}

import scala.Ordering.Implicits._
import scala.collection.mutable
import scala.io.Codec
import scala.xml.{Elem, NodeSeq}

object SchemaOrg {
  case class Node(id:String, schemaType:String, subClassOf: Seq[String], domainIncludes: Seq[String], comment: String, supersededBy: Seq[String]) {
    def toXmlSpan(toTitleCase: Boolean = true, classes: Seq[String] = Seq()): Elem = {
      val (title, seqClass) = if (supersededBy.nonEmpty) (
        "Superseded by " + supersededBy.mkString(",") + "\n" + comment,
        classes :+ "supersededBy"
      ) else (
        comment,
        classes
      )

      <span title={title} class={seqClass.mkString(" ")}>{if(toTitleCase) EnglishCaseConverter.camelCase2TitleCase(id) else id} </span>
    }
    def toLinkMarkup(implicit wikiContext:WikiContext): LinkMarkup = LinkMarkup(s"schema:$id", EnglishCaseConverter.pascalCase2TitleCase(id))
  }

  def withNameSpace(s: String): String = s"schema:$s"

  lazy val jsonTree: JsValue = Json.parse(Using(scala.io.Source.fromFile(new File("public/schema.org/tree.jsonld"))(Codec.UTF8))(_.mkString))
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


  
  lazy val jsonAllLayers: JsValue = Json.parse(Using(scala.io.Source.fromFile(new File("public/schema.org/all-layers.jsonld"))(Codec.UTF8))(_.mkString))
  lazy val seqAll:Seq[Node] = {
    val values: Seq[JsValue] = (jsonAllLayers \ "graph").as[Seq[JsValue]]
    values.map(v =>{
      val id = (v \ "id").as[String]
      val typeStr: String = getSeqString(v \ "type").find(v => v == "Class" || v == "Property").getOrElse("")
      val subClassOf: Seq[String] = getSeqString(v \ "subClassOf")
      val domainIncludes: Seq[String] = getSeqString(v \ "domainIncludes")
      val comment = (v \ "comment").as[String]
      val supersededBy: Seq[String] = getSeqString(v \ "supersededBy")
      Node(id, typeStr, subClassOf, domainIncludes, comment, supersededBy)
    })
  }
  lazy val seqClass: Seq[Node] = seqAll.filter(_.schemaType == "Class")
  lazy val seqProperty: Seq[Node] = seqAll.filter(_.schemaType == "Property")
  lazy val mapAll: Map[String, Node] = seqAll.map(n => (n.id, n)).toMap
  lazy val mapClass: Map[String, Node] = seqClass.map(n => (n.id, n)).toMap
  lazy val mapProperty: Map[String, Node] = seqProperty.map(n => (n.id, n)).toMap

  def getHtmlProperties(schema:String, seqPropertyUsed:Seq[String]): Elem = {
    val seqClass = getClassHierarchy(schema)
    <div>
      {
        seqClass.map(c => {
          <div class="properties">
            <h6>Properties of {c}</h6>
            <div>
              {
                val groupByFirstLetter: Map[Char, Seq[Node]] = seqProperty.filter(p => p.domainIncludes.contains(c)).sortBy(_.id).groupBy(p => p.id(0))
                groupByFirstLetter.keys.toSeq.sorted.map { firstLetter =>
                  <div>
                    {
                      groupByFirstLetter(firstLetter).map(p => p.toXmlSpan(toTitleCase = false, if(seqPropertyUsed.contains(p.id)){Seq("match")}else{Seq("")}))
                    }
                  </div>
                }
              }
            </div>
          </div>
        })
      }
    </div>
  }

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
