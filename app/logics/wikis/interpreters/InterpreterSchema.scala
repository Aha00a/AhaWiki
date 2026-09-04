package logics.wikis.interpreters

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.DateTimeUtil
import com.aha00a.commons.utils.EnglishCaseConverter
import com.aha00a.commons.utils.Hangul
import com.aha00a.commons.utils.UriUtil
import logics.wikis.RenderingMode
import logics.wikis.interpreters.ahaMark.AhaMarkLink
import logics.wikis.macros.MacroAttachment
import logics.wikis.macros.MacroPeriod
import models.ContextWikiPage
import models.PageContent
import play.api.libs.json.{JsArray, JsObject, JsString, JsValue, Json}

import scala.util.matching.Regex
import scala.xml.XML

object InterpreterSchema extends TraitInterpreter {

  import models.tables.CalculatedLink
  import models.tables.CalculatedSchemaOrg

  // A block may name several classes: `[[[#!Schema SoftwareApplication SoftwareSourceCode` is how
  // six pages already describe a program together with the code it is built from. Only the first
  // was read, so the rest were dropped without a word -- which is why those pages looked like they
  // were carrying properties from the wrong class.
  case class ParseResult(schemaClasses: Seq[String], seqSeqField: Seq[Seq[String]]) {
    def hasClass: Boolean = schemaClasses.nonEmpty

    /** RDFa's `typeof` takes the classes separated by spaces, so this is the attribute's value. */
    def typeofValue: String = schemaClasses.mkString(" ")
  }

  object ParseResult {
    /** One class, written as one string. Splits, so a caller may still pass "A B". */
    def apply(schemaClass: String, seqSeqField: Seq[Seq[String]]): ParseResult =
      ParseResult(schemaClass.split("""\s+""").toSeq.filter(_.isNotNullOrEmpty), seqSeqField)
  }

  def createPageContent(content: String): PageContent = {
    val pageContent: PageContent = PageContent(content)
    if (pageContent.interpreter.getOrElse("") != name)
      throw new Exception("pageContent.interpreter.getOrElse(\"\") != name")

    pageContent
  }

  def parse(pageContent: PageContent): ParseResult = {
    val schemaClasses: Seq[String] = pageContent.argument.filter(_.isNotNullOrEmpty)
    val contentLines: Seq[String] = pageContent.content.splitLinesSeq().filter(_.isNotNullOrEmpty).filterNot(_.startsWith("#"))
    val seqSeqField: Seq[Seq[String]] = contentLines.map(_.splitTabsSeq().filter(_.isNotNullOrEmpty)).filter(_.nonEmpty)
    ParseResult(schemaClasses, seqSeqField)
  }

  val mapPair: Map[String, String] = Seq(
    "birthDate" -> "deathDate",
    "startDate" -> "endDate",
    "actor" -> "character",
  ).toMap
  private val imageKeys = Set("image", "logo")
  private val urlKeys = Set("url", "codeRepository", "sameAs")
  private val locationKeys = Set("address", "geo", "location", "foundingLocation", "birthPlace")
  private val attachmentMacroRegex: Regex = """^\[\[Attachment\((.*)\)]]$""".r

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
      case (state, _) => state
    }
    if (skip) seqSeqFieldNew else seqSeqFieldNew :+ seqSeqField.last
  }

  private def isDateKey(key: String): Boolean = key.startsWith("date") || key.endsWith("Date")
  private def isLocationKey(key: String): Boolean = locationKeys.contains(key) || key.endsWith("Location")
  private def attachmentMacroArgument(v: String): Option[String] = v.trim match {
    case attachmentMacroRegex(argument) => Some(argument)
    case _ => None
  }
  private def isAttachmentMacroValue(v: String): Boolean = attachmentMacroArgument(v).isDefined
  private def normalizeUrlValue(v: String, baseUrl: Option[String]): String = {
    if (v.matches("^[\\w.+-]+://.*")) v
    else if (v.startsWith("//")) s"https:$v"
    else if (v.startsWith("/")) baseUrl.map(_ + v).getOrElse(v)
    else s"https://$v"
  }

  private def toJsonFieldValue(key: String, values: Seq[String], baseUrl: Option[String]): JsValue = {
    val normalizedValues =
      if (urlKeys.contains(key) || imageKeys.contains(key)) values.map(v => if (isAttachmentMacroValue(v)) v else normalizeUrlValue(v, baseUrl))
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
    if (!parseResult.hasClass) {
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

      // JSON-LD takes @type as a string or as an array. Keep the string for the ordinary single
      // class so the output of every existing page is untouched.
      val jsonType: JsValue = parseResult.schemaClasses match {
        case Seq(only) => JsString(only)
        case several   => JsArray(several.map(JsString.apply))
      }

      Some((Json.obj(
        "@context" -> "https://schema.org",
        "@type" -> jsonType
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

  private def renderAttachmentMacro(v: String)(implicit wikiContext: ContextWikiPage): scala.xml.NodeSeq =
    attachmentMacroArgument(v)
      .map(argument => scala.xml.Unparsed(MacroAttachment.toHtmlString(argument)))
      .getOrElse(scala.xml.NodeSeq.Empty)

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
      <dl vocab="https://schema.org/" typeof={parseResult.typeofValue}>
        <h5 class="schemaClassTitle">
          {if (!parseResult.hasClass) {
            <div class="error">Schema class is required.</div>
          } else {
            // One link per class, so both are reachable from the page that declares them.
            parseResult.schemaClasses.map(schemaClass =>
              scala.xml.XML.loadString(logics.CalculatedSchemaOrg.getSchemaClass(schemaClass).toAhaMarkLink.toHtmlString(pageNameSet)): scala.xml.NodeSeq
            ).reduce((a, b) => a ++ scala.xml.Text(" ") ++ b)}
          }
        </h5>
        <div class="schemaFields">
          {
            mergeFieldsForDisplay(parseResult.seqSeqField).map { field =>
              val key = field.key
              val tail = field.values
              val isActorCharacterField = key == "actor" && field.pairKey.contains("character")
              <div class={s"schemaFieldRow${if(isActorCharacterField) " actorCharacterPairRow" else ""}"}>
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
                        case v if isAttachmentMacroValue(v) =>
                          val className = if (imageKeys.contains(key)) "schemaFieldValue schemaFieldImage schemaFieldAttachment" else "schemaFieldValue schemaFieldAttachment"
                          <dd class={className} property={key}>{renderAttachmentMacro(v)}</dd>
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
        // Recommendations come from every declared class, deduplicated: a page typed as both a
        // program and its source should be offered the properties of both.
        val recommendedProperties = if (parseResult.hasClass) {
          val listPropCount = wikiContext.database.withConnection { implicit connection =>
            import models.tables.CalculatedSchemaOrg
            parseResult.schemaClasses.flatMap(CalculatedSchemaOrg.selectPropCountWhereCls)
          }
          listPropCount
            .filterNot(pc => seqPropertyUsed.contains(pc.prop))
            .groupMapReduce(_.prop)(_.cnt)(_ + _)
            .toSeq
            .sortBy { case (prop, cnt) => (-cnt, prop) }
            .map { case (prop, cnt) => s"$prop($cnt)" }
            .mkString(", ")
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
              {parseResult.schemaClasses.map(schemaClass =>
                <div>
                  {logics.CalculatedSchemaOrg.getHtmlTree(schemaClass)}
                  {
                    if(logics.CalculatedSchemaOrg.mapClass.isDefinedAt(schemaClass)) {
                      <div>{logics.CalculatedSchemaOrg.getHtmlProperties(schemaClass, seqPropertyUsed)}</div>
                    } else {

                    }
                  }
                </div>
              )}
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

    val seqKeyValue: Seq[(String, String)] = parseResult.seqSeqField
      .collect { case key +: values if values.nonEmpty => key +: values }
      .filterNot {
        case _ +: value +: _ => value.startsWith("http://") || value.startsWith("https://")
        case _ => false
      }
      .flatMap {
        case key +: tail if locationKeys.contains(key) =>
          tail.flatMap(toAddressCandidates).map(v => key -> v)
        case key +: tail =>
          tail.flatMap(DateTimeUtil.expand_ymd_to_ymd_ym).map(v => key -> v)
      }

    // A row per class, so a page declaring both is listed under both on schema:Schema rather than
    // only under whichever was written first. cls is part of the key, so the copies do not collide.
    // A block with no class keeps producing its one empty-class row, as before.
    val seqSchemaClass: Seq[String] = if (parseResult.hasClass) parseResult.schemaClasses else Seq("")
    seqSchemaClass.flatMap(schemaClass =>
      CalculatedSchemaOrg(wikiContext.name, schemaClass, "", s"schema:$schemaClass") +:
        seqKeyValue.map { case (key, value) => CalculatedSchemaOrg(wikiContext.name, schemaClass, key, value) }
    )
  }
}
