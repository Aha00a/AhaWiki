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
import play.api.libs.json.{JsArray, JsObject, JsString, JsValue, Json}

import scala.xml.XML

object InterpreterSchema extends TraitInterpreter {

  import models.tables.CalculatedLink
  import models.tables.CalculatedSchemaOrg

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
  private val imageKeys = Set("image", "logo")
  private val urlKeys = Set("url", "codeRepository", "sameAs")
  private val locationKeys = Set("address", "geo", "location", "foundingLocation")

  case class DisplayField(key: String, values: Seq[String], pairKey: Option[String] = None, pairValues: Seq[String] = Seq.empty)

  def mergeFieldsForDisplay(seqSeqField: Seq[Seq[String]], mapPair: Map[String, String] = mapPair): Seq[DisplayField] = {
    val buffer = scala.collection.mutable.ArrayBuffer.empty[DisplayField]
    var i = 0
    while (i < seqSeqField.size) {
      val seqNow = seqSeqField(i)
      val key = seqNow.head
      val values = seqNow.tail

      val merged = mapPair.get(key)
        .filter(_ => i + 1 < seqSeqField.size)
        .flatMap { pairKey =>
          val seqNext = seqSeqField(i + 1)
          if (seqNext.head == pairKey && values.size == seqNext.tail.size) {
            Some(DisplayField(key, values, Some(pairKey), seqNext.tail))
          } else {
            None
          }
        }

      merged match {
        case Some(v) =>
          buffer += v
          i += 2
        case None =>
          buffer += DisplayField(key, values)
          i += 1
      }
    }
    buffer.toSeq
  }

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

  private def isDateKey(key: String): Boolean = key.startsWith("date") || key.endsWith("Date")
  private def isLocationKey(key: String): Boolean = locationKeys.contains(key) || key.endsWith("Location")
  private def normalizeUrlValue(v: String, baseUrl: Option[String]): String = {
    if (v.matches("^[\\w.+-]+://.*")) v
    else if (v.startsWith("//")) s"https:$v"
    else if (v.startsWith("/")) baseUrl.map(_ + v).getOrElse(v)
    else s"https://$v"
  }

  private def toJsonFieldValue(key: String, values: Seq[String], baseUrl: Option[String]): JsValue = {
    val normalizedValues =
      if (urlKeys.contains(key) || imageKeys.contains(key)) values.map(v => normalizeUrlValue(v, baseUrl))
      else values

    val normalizedDistinct = normalizedValues.distinct
    val convertedValues: Seq[JsValue] = key match {
      case "address" =>
        normalizedDistinct.map(v => Json.obj("@type" -> "PostalAddress", "streetAddress" -> v))
      case "location" | "foundingLocation" =>
        normalizedDistinct.map(v => Json.obj("@type" -> "Place", "name" -> v))
      case "geo" =>
        normalizedDistinct.map { v =>
          v.split(",").map(_.trim).toSeq match {
            case Seq(lat, lng) if lat.matches("""^-?\d+(\.\d+)?$""") && lng.matches("""^-?\d+(\.\d+)?$""") =>
              Json.obj("@type" -> "GeoCoordinates", "latitude" -> lat, "longitude" -> lng)
            case _ =>
              JsString(v)
          }
        }
      case _ =>
        normalizedDistinct.map(JsString)
    }

    if (convertedValues.size == 1) convertedValues.head else JsArray(convertedValues)
  }

  def toJsonLdObject(
    parseResult: ParseResult,
    baseUrl: Option[String] = None,
    pageUrl: Option[String] = None,
    pageName: Option[String] = None,
    language: Option[String] = None
  ): Option[JsObject] = {
    if (parseResult.schemaClass.isNullOrEmpty) {
      None
    } else {
      val fields = parseResult.seqSeqField
        .collect { case key +: values if values.nonEmpty => key -> values }
        .groupMap(_._1)(_._2)
        .map { case (key, groupedValues) => key -> groupedValues.flatten }
      val properties: Seq[(String, JsValue)] = fields.toSeq.map { case (key, values) => key -> toJsonFieldValue(key, values, baseUrl) }
      val defaultProperties = Seq(
        pageUrl.filter(_ => !fields.contains("url")).map("url" -> JsString(_)),
        pageUrl.map("mainEntityOfPage" -> JsString(_)),
        pageUrl.map("@id" -> JsString(_)),
        pageName.filter(_ => !fields.contains("name")).map("name" -> JsString(_)),
        language.map("inLanguage" -> JsString(_)),
      ).flatten

      Some((Json.obj(
        "@context" -> "https://schema.org",
        "@type" -> parseResult.schemaClass
      ) ++ JsObject(defaultProperties ++ properties)))
    }
  }

  private def renderPropertyTitle(key: String, pairKey: Option[String]): scala.xml.NodeSeq = {
    (logics.CalculatedSchemaOrg.mapProperty.get(key), pairKey.flatMap(logics.CalculatedSchemaOrg.mapProperty.get)) match {
      case (Some(keySchema), Some(pairSchema)) =>
        <span>{keySchema.toXmlSpan()} / {pairSchema.toXmlSpan()}</span>
      case (Some(keySchema), None) =>
        keySchema.toXmlSpan()
      case (None, Some(_)) =>
        <span class="unknown" title="Unknown property">{EnglishCaseConverter.camelCase2TitleCase(key)} / {EnglishCaseConverter.camelCase2TitleCase(pairKey.get)}</span>
      case (None, None) =>
        <span class="unknown" title="Unknown property">{EnglishCaseConverter.camelCase2TitleCase(key)}</span>
    }
  }

  private def toAddressCandidates(v: String): Seq[String] =
    if (Hangul.containsKo(v)) expandAddress(v).map(_.mkString(" ")) else Seq(v)

  private def mapAndWrapLink(v: String, pageNameSet: Set[String])(implicit contextWikiPage: ContextWikiPage): scala.xml.NodeSeq =
    XML.loadString(AhaMarkLink(v).toHtmlString(pageNameSet))

  private def mapAndWrapLink(link: AhaMarkLink, pageNameSet: Set[String]): scala.xml.NodeSeq =
    XML.loadString(link.toHtmlString(pageNameSet))


  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = {
    import models.tables.Site
    implicit val site: Site = wikiContext.site
    val pageContent: PageContent = createPageContent(content)
    val parseResult: ParseResult = parse(pageContent)

    val pageNameSet: Set[String] = wikiContext.setPageNameByPermission
    val baseUrl: Option[String] = Option(wikiContext.requestWrapper.host).filter(_.isNotNullOrEmpty).map(host => s"https://$host")
    val pageUrl: Option[String] = baseUrl.map(base => s"$base/w/${UriUtil.encodeURIComponent(wikiContext.name)}")

    val seqPropertyUsed: Seq[String] = parseResult.seqSeqField.flatMap(_.headOption)
//        <h5>
//          {
//            logics.CalculatedSchemaOrg.getPathHierarchy(parseResult.schemaClass).map(seqClass => {
//              scala.xml.XML.loadString(
//                seqClass.map(c => logics.CalculatedSchemaOrg.mapClass.get(c)
//                  .map(schemaType => schemaType.toAhaMarkLink.toHtmlString(pageNameSet))
//                  .getOrElse("")
//                ).mkString("<div>", " / ", "</div>")
//              )
//            })
//          }
//        </h5>

    val dl =
      <dl vocab="https://schema.org/" typeof={parseResult.schemaClass}>
        <h5 class="schemaClassTitle">
          {if (parseResult.schemaClass.isNullOrEmpty) {
            <div class="error">TODO: Specify Schema Class</div>
          } else {
            scala.xml.XML.loadString(logics.CalculatedSchemaOrg.getSchemaClass(parseResult.schemaClass).toAhaMarkLink.toHtmlString(pageNameSet))}
          }
        </h5>
        <div class="schemaFields">
          {
            mergeFieldsForDisplay(parseResult.seqSeqField).map { field =>
              val key = field.key
              val tail = field.values
              <div class="schemaFieldRow">
                <dt class="schemaFieldKey">
                  {renderPropertyTitle(key, field.pairKey)}
                </dt>
                {
                  field.pairKey match {
                    case Some(pairKey) =>
                      tail.zip(field.pairValues).map {
                        case (v1, v2) =>
                          <dd class="schemaFieldValue schemaFieldValuePair">
                            <span property={key}>{XML.loadString(AhaMarkLink(v1).toHtmlString(pageNameSet))}</span>
                            <span class="pairSeparator"> / </span>
                            <span property={pairKey}>{XML.loadString(AhaMarkLink(v2).toHtmlString(pageNameSet))}</span>
                          </dd>
                      }
                    case None =>
                      tail.map {
                        case v if imageKeys.contains(key) =>
                          <dd class="schemaFieldValue schemaFieldImage" property={key}><img src={normalizeUrlValue(v, baseUrl)} alt={s"${wikiContext.name} $key"}></img></dd>
                        case v if urlKeys.contains(key) =>
                          <dd class="schemaFieldValue" property={key}>{XML.loadString(AhaMarkLink(normalizeUrlValue(v, baseUrl)).toHtmlString(pageNameSet))}</dd>
                        case v if isDateKey(key) =>
                          <dd class="schemaFieldValue" property={key}>
                            {XML.loadString(AhaMarkLink(v).toHtmlString(pageNameSet))}
                            <span class="datePeriod">({MacroPeriod.toHtmlString(v)})</span>
                          </dd>
                        case v if isLocationKey(key) =>
                          val mapJavaScriptApiKey = wikiContext.applicationConf.AhaWiki.google.credentials.api.MapsJavaScriptAPI.key()
                          <div class="address">
                            <dd property={key}>
                              {
                                if(Hangul.containsKo(v)) {
                                  expandAddress(v).flatMap(seq => Seq(
                                    mapAndWrapLink(AhaMarkLink(seq.mkString(" "), seq.last), pageNameSet),
                                    " ",
                                  ))
                                } else {
                                  mapAndWrapLink(v, pageNameSet)
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
                          <dd class="schemaFieldValue" property={key}>{mapAndWrapLink(v, pageNameSet)}</dd>
                      }
                  }
                }
              </div>
            }
          }
        </div>
      </dl>
    val jsonLdScript = toJsonLdObject(
      parseResult = parseResult,
      baseUrl = baseUrl,
      pageUrl = pageUrl,
      pageName = Some(wikiContext.name),
      language = Some(wikiContext.requestWrapper.locale.toLanguageTag),
    ).map(json =>
      <script type="application/ld+json">{scala.xml.Unparsed(Json.stringify(json))}</script>
    )
    wikiContext.renderingMode match {
      case RenderingMode.Normal =>
        val r = <div class="schema InterpreterSchema">{dl}{jsonLdScript.getOrElse(scala.xml.NodeSeq.Empty)}</div>
        r.toString()
      case RenderingMode.Preview =>
        val recommendedProperties = if (parseResult.schemaClass.isNotNullOrEmpty){
          val listPropCount = wikiContext.database.withConnection { implicit connection =>
            import models.tables.CalculatedSchemaOrg
            CalculatedSchemaOrg.selectPropCountWhereCls(parseResult.schemaClass)
          }
          listPropCount.filterNot(pc => seqPropertyUsed.contains(pc.prop)).map(pc => s"${pc.prop}(${pc.cnt})").mkString(", ")
        } else {
          ""
        }
        val r =
          <div class="schema InterpreterSchema">
            {dl}
            {jsonLdScript.getOrElse(scala.xml.NodeSeq.Empty)}
            <div class="preview info">
              <h6>Recommended Properties</h6>
              {recommendedProperties}
              <h6>Hierarchical Search</h6>
              {logics.CalculatedSchemaOrg.getHtmlTree(parseResult.schemaClass)}
              {
                if(logics.CalculatedSchemaOrg.mapClass.isDefinedAt(parseResult.schemaClass)) {
                  <div>{logics.CalculatedSchemaOrg.getHtmlProperties(parseResult.schemaClass, seqPropertyUsed)}</div>
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

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] = Seq()

  override def toSeqSchemaOrg(content: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedSchemaOrg] = {
    val pageContent: PageContent = createPageContent(content)
    val parseResult: ParseResult = parse(pageContent)

    val seqLinkProperty: Seq[CalculatedSchemaOrg] = parseResult.seqSeqField
      .collect { case key +: values if values.nonEmpty => key +: values }
      .filterNot {
        case _ +: value +: _ => value.startsWith("http://") || value.startsWith("https://")
        case _ => false
      }
      .flatMap {
        case key +: tail if locationKeys.contains(key) =>
          tail.flatMap(toAddressCandidates).map(v => CalculatedSchemaOrg(wikiContext.name, parseResult.schemaClass, key, v))
        case key +: tail =>
          tail
            .flatMap(DateTimeUtil.expand_ymd_to_ymd_ym)
            .map(CalculatedSchemaOrg(wikiContext.name, parseResult.schemaClass, key, _))
      }
    CalculatedSchemaOrg(wikiContext.name, parseResult.schemaClass, "", s"schema:${parseResult.schemaClass}") +: seqLinkProperty
  }
}
