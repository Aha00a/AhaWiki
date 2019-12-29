package logics

import java.io.File

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.Using
import play.api.libs.json.{JsLookupResult, JsValue, Json}

import scala.io.Codec
import scala.xml.{Elem, NodeSeq}

object Schema {
  def withNameSpace(s: String): String = s"schema:$s"

  implicit val codec: Codec = Codec.UTF8

  lazy val jsonTree: JsValue = Json.parse(Using(scala.io.Source.fromFile(new File("public/schema.org/tree.jsonld")))(_.mkString))

  def getHtmlTree(q:String, node:JsValue = jsonTree): NodeSeq = {
    val id = (node \ "id").as[String]
    val idWithNameSpace = withNameSpace(id)
    val children = (node \ "children").asOpt[Seq[JsValue]]
    if(id.containsIgnoreCase(q)) {
      <ul>
        <li><a href={s"/w/${idWithNameSpace}"}>{id}</a></li>
        {children.map(seq => seq.map(n => getHtmlTree("", n))).getOrElse(NodeSeq.Empty)}
      </ul>
    } else {
      val c: Seq[xml.Node] = children.map(seq => seq.map(n => getHtmlTree(q, n))).getOrElse(NodeSeq.Empty).flatten
      if(c.isEmpty) {
        c
      } else {
        <ul>
          <li><a href={s"/w/${idWithNameSpace}"}>{id}</a></li>
          {c}
        </ul>
      }
    }
  }

  private val file: String = Using(scala.io.Source.fromFile(new File("public/schema.org/all-layers.jsonld")))(_.mkString)
  def jsonAllLayers: JsValue = Json.parse(file)

  case class Node(id:String, schemaType:String, subClassOf: Seq[String], domainIncludes: Seq[String], comment: String, supersededBy: Seq[String]) {
    def toXmlSpan(classes: String*): Elem = {
      <span
        title={
          Seq(
            if(supersededBy.isEmpty) "" else "Superseded by " + supersededBy.mkString(","),
            comment
          ).filter(_.isNotNullOrEmpty).mkString("\n")
        }
        class={
          (classes :+ (if(supersededBy.nonEmpty) "supersededBy" else "")).mkString(" ")
        }
      >{id} </span>
    }
  }
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
  lazy val mapClass: Map[String, Node] = seqClass.map(n => (n.id, n)).toMap
  lazy val seqProperty: Seq[Node] = seqAll.filter(_.schemaType == "Property")
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
                      groupByFirstLetter(firstLetter).map(p => p.toXmlSpan(if(seqPropertyUsed.contains(p.id)){"match"}else{""}))
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

  def getSeqString(jsLookupResult: JsLookupResult): Seq[String] = {
    None
      .orElse(jsLookupResult.asOpt[String].map(v => Seq(v)))
      .orElse(jsLookupResult.asOpt[Seq[String]])
      .getOrElse(Seq[String]())
  }

}
