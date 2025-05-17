package logics.wikis.interpreters

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.DateTimeUtil
import com.aha00a.commons.utils.EnglishCaseConverter
import com.aha00a.commons.utils.Hangul
import com.aha00a.commons.utils.UriUtil
import logics.wikis.RenderingMode
import logics.wikis.interpreters.ahaMark.AhaMarkLink
import logics.wikis.macros.MacroPeriod
import models.ContextWikiPage
import models.PageContent

import scala.xml.XML

object InterpreterSchema extends TraitInterpreter {

  import models.tables.Link
  import models.tables.SchemaOrg

  case class ParseResult(schemaClass: String, seqSeqField: Seq[Seq[String]])

  def createPageContent(content: String): PageContent = {
    val pageContent: PageContent = PageContent(content)
    if (pageContent.interpreter.getOrElse("") != name)
      throw new Exception("pageContent.interpreter.getOrElse(\"\") != name")

    pageContent
  }

  def parse(pageContent: PageContent): ParseResult = {
    val schemaClass: String = pageContent.argument.headOption.getOrElse("")
    val contentLines: Seq[String] = pageContent.content.splitLinesSeq().filter(_.isNotNullOrEmpty).filterNot(_.startsWith("#"))
    val seqSeqField: Seq[Seq[String]] = contentLines.map(_.splitTabsSeq().filter(_.isNotNullOrEmpty)).filter(_.nonEmpty)
// TODO:    ParseResult(schemaClass, mergeFields(seqSeqField))
    ParseResult(schemaClass, seqSeqField)
  }

  val mapPair: Map[String, String] = Seq(
    "birthDate" -> "deathDate",
    "startDate" -> "endDate",
    "actor" -> "character",
  ).toMap

  def mergeFields(seqSeqField: Seq[Seq[String]], mapPair: Map[String, String] = mapPair): Seq[Seq[String]] = {
    val (seqSeqFieldNew, skip) = seqSeqField.sliding(2).foldLeft((Seq.empty[Seq[String]], false)) {
      case ((acc, false), Seq(_)) => (acc, false)
      case ((acc, true), Seq(_, _)) => (acc, false)
      case ((acc, false), Seq(seqNow, seqNext)) =>
        val key1 = seqNow.head
        val key2 = seqNext.head

        mapPair.get(key1).filter(_ == key2) match {
          case Some(_) =>
            val mergedValues = seqNow.tail.zipAll(seqNext.tail, "", "").flatMap { case (a, b) => Seq(a, b) }
            (acc :+ (s"$key1 $key2" +: mergedValues), true)
          case None =>
            (acc :+ seqNow, false)
        }
    }
    if (skip) seqSeqFieldNew else seqSeqFieldNew :+ seqSeqField.last
  }


  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = {
    import models.tables.Site
    implicit val site: Site = wikiContext.site
    val pageContent: PageContent = createPageContent(content)
    val parseResult: ParseResult = parse(pageContent)

    val pageNameSet: Set[String] = wikiContext.setPageNameByPermission

    val seqPropertyUsed: Seq[String] = parseResult.seqSeqField.flatMap(_.headOption)
//        <h5>
//          {
//            logics.SchemaOrg.getPathHierarchy(parseResult.schemaClass).map(seqClass => {
//              scala.xml.XML.loadString(
//                seqClass.map(c => logics.SchemaOrg.mapClass.get(c)
//                  .map(schemaType => schemaType.toAhaMarkLink.toHtmlString(pageNameSet))
//                  .getOrElse("")
//                ).mkString("<div>", " / ", "</div>")
//              )
//            })
//          }
//        </h5>

    val dl =
      <dl vocab="http://schema.org/" typeof={parseResult.schemaClass}>
        <h5>
          {if (parseResult.schemaClass.isNullOrEmpty) {
            <div class="error">TODO: Specify Schema Class</div>
          } else {
            scala.xml.XML.loadString(logics.SchemaOrg.getSchemaClass(parseResult.schemaClass).toAhaMarkLink.toHtmlString(pageNameSet))}
          }
        </h5>
        <div>
          {
            parseResult.seqSeqField.map { case key +: tail =>
              <div>
                <dt>
                  {
                  logics.SchemaOrg.mapProperty.get(key).map(n => {
                      n.toXmlSpan()
                    }).getOrElse{
                      <span class="unknown" title="Unknown property">{EnglishCaseConverter.camelCase2TitleCase(key)}</span>
                    }
                  }
                </dt>
                {
                  tail.map {
                    case v if Seq("image", "logo").contains(key) =>
                      <dd property={key}><img src={v} alt={s"$v $key"}></img></dd>
                    case v if Seq("url", "codeRepository", "sameAs").contains(key) =>
                      <dd property={key}>{XML.loadString(AhaMarkLink(if (v.matches("^[\\w.+-]+://.*")) v else "https://" + v).toHtmlString(pageNameSet))}</dd>
                    case v if key.startsWith("date") || key.endsWith("Date") =>
                      <dd property={key}>
                        {XML.loadString(AhaMarkLink(v).toHtmlString(pageNameSet))}
                        ({MacroPeriod.toHtmlString(v)})
                      </dd>
                    case v if key.startsWith("address") || key == "geo" || key == "location" || key.endsWith("Location") =>
                      val mapJavaScriptApiKey = wikiContext.applicationConf.AhaWiki.google.credentials.api.MapsJavaScriptAPI.key()
                      <div class="address">
                        <dd property={key}>
                          {
                            if(Hangul.containsKo(v)) {
                              expandAddress(v).flatMap(seq => Seq(
                                XML.loadString(AhaMarkLink(seq.mkString(" "), seq.last).toHtmlString(pageNameSet)),
                                " ",
                              ))
                            } else {
                              XML.loadString(AhaMarkLink(v).toHtmlString(pageNameSet))
                            }
                          }
                          <div class="aspectRatioWrapper">
                            <div class="ratio_1_1" ></div>
                            <div class="aspectRatioContent">
                              <iframe
                              width="100%" height="100%" frameborder="0"
                              allowfullscreen="allowfullscreen"
                              src={s"https://www.google.com/maps/embed/v1/place?q=${UriUtil.encodeURIComponent(v)}&key=${mapJavaScriptApiKey}"}></iframe>
                            </div>
                          </div>
                          {
                            if (Hangul.containsKo(v)) {
                              <div class="mapServiceLinks">
                                <a rel="noopener" target="_blank" href={s"https://www.google.com/maps/search/${UriUtil.encodeURIComponent(v)}?hl=en&source=opensearch"}><img class="iconMap" src="/public/img/GoogleMap.ico" alt="Google Map"/>Google&nbsp;Map</a>
                                <a rel="noopener" target="_blank" href={s"https://map.naver.com/p/search/${UriUtil.encodeURIComponent(v)}"}><img class="iconMap" src="/public/img/NaverMap.ico" alt="Naver Map"/>Naver&nbsp;Map</a>
                                <a rel="noopener" target="_blank" href={s"http://map.daum.net/?q=${UriUtil.encodeURIComponent(v)}"}><img class="iconMap" src="/public/img/KakaoMap.ico" alt="KakaoMap"/>Kakao&nbsp;Map</a>
                              </div>
                            }
                          }
                        </dd>
                      </div>
                    case v =>
                      <dd property={key}>{XML.loadString(AhaMarkLink(v).toHtmlString(pageNameSet))}</dd>
                  }
                }
              </div>
            }
          }
        </div>
      </dl>
    wikiContext.renderingMode match {
      case RenderingMode.Normal =>
        val r = <div class="schema InterpreterSchema">{dl}</div>
        r.toString()
      case RenderingMode.Preview =>
        val recommendedProperties = if (parseResult.schemaClass.isNotNullOrEmpty){
          val listPropCount = wikiContext.database.withConnection { implicit connection =>
            import models.tables.SchemaOrg
            SchemaOrg.selectPropCountWhereCls(parseResult.schemaClass)
          }
          listPropCount.filterNot(pc => seqPropertyUsed.contains(pc.prop)).map(pc => s"${pc.prop}(${pc.cnt})").mkString(", ")
        } else {
          ""
        }
        val r =
          <div class="schema InterpreterSchema">
            {dl}
            <div class="preview info">
              <h6>Recommended Properties</h6>
              {recommendedProperties}
              <h6>Hierarchical Search</h6>
              {logics.SchemaOrg.getHtmlTree(parseResult.schemaClass)}
              {
                if(logics.SchemaOrg.mapClass.isDefinedAt(parseResult.schemaClass)) {
                  <div>{logics.SchemaOrg.getHtmlProperties(parseResult.schemaClass, seqPropertyUsed)}</div>
                } else {

                }
              }
            </div>
          </div>
        r.toString()
    }
  }

  def expandAddress(v: String): Seq[Seq[String]] = {
    v.split("\\s+")
      .filter(_.isNotNullOrEmpty)
      .indices
      .map(i => v
        .split("\\s+")
        .filter(_.isNotNullOrEmpty)
        .take(i + 1)
        .toSeq
      )
  }

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[Link] = Seq()

  override def toSeqSchemaOrg(content: String)(implicit wikiContext: ContextWikiPage): Seq[SchemaOrg] = {
    val pageContent: PageContent = createPageContent(content)
    val parseResult: ParseResult = parse(pageContent)

    val seqLinkProperty: Seq[SchemaOrg] = parseResult.seqSeqField
      .filterNot(_(1).startsWith("http://"))
      .filterNot(_(1).startsWith("https://"))
      .flatMap {
        case "address" +: tail =>
          val seq: Seq[String] = tail.flatMap(v => if (Hangul.containsKo(v)) expandAddress(v).map(_.mkString(" ")) else Seq(v))
          seq.map(v => SchemaOrg(wikiContext.name, parseResult.schemaClass, "address", v))
        case "geo" +: tail =>
          val seq: Seq[String] = tail.flatMap(v => if (Hangul.containsKo(v)) expandAddress(v).map(_.mkString(" ")) else Seq(v))
          seq.map(v => SchemaOrg(wikiContext.name, parseResult.schemaClass, "geo", v))
        case "location" +: tail =>
          val seq: Seq[String] = tail.flatMap(v => if (Hangul.containsKo(v)) expandAddress(v).map(_.mkString(" ")) else Seq(v))
          seq.map(v => SchemaOrg(wikiContext.name, parseResult.schemaClass, "location", v))
        case "foundingLocation" +: tail =>
          val seq: Seq[String] = tail.flatMap(v => if (Hangul.containsKo(v)) expandAddress(v).map(_.mkString(" ")) else Seq(v))
          seq.map(v => SchemaOrg(wikiContext.name, parseResult.schemaClass, "foundingLocation", v))
        case key +: tail =>
          tail
            .flatMap(DateTimeUtil.expand_ymd_to_ymd_ym)
            .map(SchemaOrg(wikiContext.name, parseResult.schemaClass, key, _))
      }
    SchemaOrg(wikiContext.name, parseResult.schemaClass, "", s"schema:${parseResult.schemaClass}") +: seqLinkProperty
  }
}
