package logics.wikis

import java.time.LocalDate
import scala.util.Try

/**
 * The part of `#!Query` that is a question about data, with no database and no HTML in it.
 *
 * A page's `#!Schema` blocks are rows of `(page, prop, value)` in `CalculatedSchemaOrg`. A query
 * says which pages to keep (`where`), which of their properties to show, how to order and group
 * them, and how many to take. It is the same idea as a Notion database view or an Obsidian Base,
 * over data the wiki already has — nothing is entered twice.
 *
 * A property can hold several values (`sameAs` does), so every condition is about the values as a
 * set: `=` holds when any value is equal, `!=` when none is. That is the question people mean, and
 * the one that does not change a page's answer when it gains a second value.
 */
object QueryEngine {
  val DefaultLimit = 100
  val MaxLimit = 500
  val Views: Set[String] = Set("table", "list")

  case class Condition(prop: String, op: String, value: String)

  case class Spec(
    classes: Seq[String],
    conditions: Seq[Condition],
    show: Seq[String],
    sortBy: Option[(String, Boolean)],
    group: Option[String],
    limit: Int,
    view: String,
  ) {
    /** Every property the query reads — what the database has to be asked for. */
    def props: Seq[String] = (conditions.map(_.prop) ++ show ++ sortBy.map(_._1) ++ group).distinct
  }

  /** One page that answered, with the values of the properties the query read. */
  case class Row(page: String, values: Map[String, Seq[String]]) {
    def first(prop: String): Option[String] = values.get(prop).flatMap(_.headOption)
  }

  case class Result(rows: Seq[Row], total: Int)

  private val ValueOps = Set("=", "!=", "~", "!~", "<", "<=", ">", ">=", "md")
  private val BareOps = Set("has", "none")

  /**
   * The spec a block's header and body lines describe.
   *
   * `lines` are the body's tab-separated cells, as `InterpreterSchema.parse` reads them. A line this
   * does not know is an error rather than ignored: a misspelt `shwo` that silently showed nothing
   * would look like a query that matched nothing.
   */
  def parse(classes: Seq[String], lines: Seq[Seq[String]]): Either[String, Spec] = {
    var spec = Spec(classes, Seq.empty, Seq.empty, None, None, DefaultLimit, "table")
    val errors = Seq.newBuilder[String]

    lines.foreach {
      case Seq("where", prop, op) if BareOps.contains(op) =>
        spec = spec.copy(conditions = spec.conditions :+ Condition(prop, op, ""))
      case Seq("where", prop, op, value) if ValueOps.contains(op) =>
        spec = spec.copy(conditions = spec.conditions :+ Condition(prop, op, value))
      case "where" +: rest =>
        errors += s"where takes `prop`, then ${BareOps.mkString("/")}, or one of ${ValueOps.mkString(" ")} and a value: ${rest.mkString(" ")}"
      case "show" +: props if props.nonEmpty =>
        spec = spec.copy(show = spec.show ++ props)
      case Seq("sort", prop) =>
        spec = spec.copy(sortBy = Some((prop, false)))
      case Seq("sort", prop, direction) if Set("asc", "desc").contains(direction) =>
        spec = spec.copy(sortBy = Some((prop, direction == "desc")))
      case Seq("group", prop) =>
        spec = spec.copy(group = Some(prop))
      case Seq("limit", n) =>
        Try(n.toInt).toOption.filter(_ >= 1) match {
          case Some(v) => spec = spec.copy(limit = v.min(MaxLimit))
          case None => errors += s"limit takes a number of at least 1: $n"
        }
      case Seq("view", v) if Views.contains(v) =>
        spec = spec.copy(view = v)
      case Seq("view", v) =>
        errors += s"view is one of ${Views.toSeq.sorted.mkString(", ")}: $v"
      case other =>
        errors += s"Not a line a query knows: ${other.mkString(" ")}"
    }

    val found = errors.result()
    if (found.nonEmpty) Left(found.mkString("; "))
    else if (spec.classes.isEmpty && spec.conditions.isEmpty) Left("Name a class (`#!Query Person`) or give a `where` line: a query with neither would be every page.")
    else Right(spec)
  }

  /**
   * Run `spec` over `facts` — page to property to its values.
   *
   * `inClass` is the pages that belong to the query's classes, or None when it named none, in
   * which case every page in `facts` is a candidate and `where` alone decides.
   */
  def run(spec: Spec, facts: Map[String, Map[String, Seq[String]]], inClass: Option[Set[String]], today: LocalDate): Result = {
    val candidates = inClass.map(_.toSeq).getOrElse(facts.keys.toSeq)
    val kept = candidates.filter(page => spec.conditions.forall(holds(_, facts.getOrElse(page, Map.empty), today)))

    val rows = kept.map(page => Row(page, facts.getOrElse(page, Map.empty)))
    val ordered = order(rows, spec)
    Result(ordered.take(spec.limit), ordered.size)
  }

  private def order(rows: Seq[Row], spec: Spec): Seq[Row] = {
    // Group first, then the sort inside each group; pages without the grouped property come last.
    val sorted = spec.sortBy match {
      case Some((prop, descending)) =>
        // A page with no value sorts last whichever way the order runs: it is missing, not smallest.
        val (having, lacking) = rows.partition(_.first(prop).isDefined)
        // Ties are by name, whichever way the order runs, so a result does not shuffle between renders.
        val ordered = having.sortWith { (a, b) =>
          val c = KeyOrdering.compare(sortKey(a.first(prop).get), sortKey(b.first(prop).get))
          if (c == 0) a.page < b.page else if (descending) c > 0 else c < 0
        }
        ordered ++ lacking.sortBy(_.page)
      case None => rows.sortBy(_.page)
    }
    spec.group match {
      case Some(prop) =>
        val (having, lacking) = sorted.partition(_.first(prop).isDefined)
        having.groupBy(_.first(prop).get).toSeq.sortBy(_._1).flatMap(_._2) ++ lacking
      case None => sorted
    }
  }

  /** Numbers compare as numbers, everything else as text — so dates in ISO form sort as dates. */
  private sealed trait Key
  private case class Num(n: BigDecimal) extends Key
  private case class Text(s: String) extends Key

  private def sortKey(value: String): Key =
    Try(BigDecimal(value.trim)).toOption.map(Num).getOrElse(Text(value.toLowerCase))

  private object KeyOrdering extends Ordering[Key] {
    override def compare(a: Key, b: Key): Int = (a, b) match {
      case (Num(x), Num(y)) => x.compare(y)
      case (Text(x), Text(y)) => x.compare(y)
      case (Num(_), Text(_)) => -1 // numbers before text, so a column of mixed values is at least consistent
      case (Text(_), Num(_)) => 1
    }
  }

  /** `today` stands for the date in a value: an ISO date, or `--MM-DD` for `md`. */
  private def resolve(condition: Condition, today: LocalDate): String =
    if (condition.value == "today") {
      if (condition.op == "md") f"--${today.getMonthValue}%02d-${today.getDayOfMonth}%02d" else today.toString
    } else condition.value

  private val MonthDay = """^(?:\d{4}|-)-(\d{2})-(\d{2})$""".r

  private def monthDayOf(value: String): Option[(String, String)] = value match {
    case MonthDay(m, d) => Some((m, d))
    case _ => None
  }

  private def holds(condition: Condition, values: Map[String, Seq[String]], today: LocalDate): Boolean = {
    val have = values.getOrElse(condition.prop, Seq.empty)
    val wanted = resolve(condition, today)
    condition.op match {
      case "has" => have.nonEmpty
      case "none" => have.isEmpty
      case "=" => have.exists(_.equalsIgnoreCase(wanted))
      case "!=" => !have.exists(_.equalsIgnoreCase(wanted))
      case "~" => have.exists(_.toLowerCase.contains(wanted.toLowerCase))
      case "!~" => !have.exists(_.toLowerCase.contains(wanted.toLowerCase))
      case "md" =>
        // The same month and day in any year, or in none: `--10-23` and `1815-10-23` are both October 23rd.
        val target = monthDayOf(wanted)
        target.isDefined && have.exists(v => monthDayOf(v) == target)
      case "<" | "<=" | ">" | ">=" =>
        have.exists { v =>
          val c = KeyOrdering.compare(sortKey(v), sortKey(wanted))
          condition.op match { case "<" => c < 0; case "<=" => c <= 0; case ">" => c > 0; case _ => c >= 0 }
        }
      case _ => false
    }
  }
}
