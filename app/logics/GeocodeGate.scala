package logics

import logics.GeocodeGate._

import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration

/**
 * Which addresses are asked of the Geocoding API now, and which are left for later.
 *
 * A `#!Map` block asks for the coordinates of every address it does not have yet, each time it is
 * drawn, and a block is drawn for anyone: a preview needs no login. Every question is a paid call
 * on the instance's one key. Until 2026-11-21 nothing stood between the two -- a posted preview
 * with as many made-up addresses as two megabytes hold asked every one of them, an address the
 * API has no place for was asked again at every view of its page, for ever, and the same address
 * was asked twice when a page was drawn twice before the first answer came.
 *
 * So, in the one place every question passes through (ServiceGeocode):
 *  - an address is asked once while its answer is on the way;
 *  - an address the API has no place for is left alone for `retryUnresolvedAfter`, and one whose
 *    lookup failed for another reason (the network, a quota) for the much shorter
 *    `retryErrorAfter` -- the first is the address's fault and the second is not;
 *  - at most `maxPerDay` questions are asked in any twenty-four hours. Past that an address is
 *    simply not asked: its marker is missing until the page is drawn on a later day.
 *
 * It holds something only about a question it let through, so what it holds is bounded by the
 * budget, however many addresses it is shown. It is one process's memory: during a deploy the two
 * instances each have their own, and a restart forgets it.
 *
 * Not thread-confined: the actor asks, and the answers come back on other threads.
 */
final class GeocodeGate(
  maxPerDay: Int,
  retryUnresolvedAfter: FiniteDuration,
  retryErrorAfter: FiniteDuration,
  nowMillis: () => Long = () => System.currentTimeMillis(),
) {
  private val day: Long = 24L * 60 * 60 * 1000
  // When each question of the last twenty-four hours was let through, oldest first.
  private val asked = mutable.Queue[Long]()
  private val pending = mutable.Set[String]()
  // An address that failed, and until when it is left alone.
  private val failedUntil = mutable.Map[String, Long]()
  private var saidOverBudget = false

  def decide(address: String): Decision = synchronized {
    val now = nowMillis()
    while (asked.headOption.exists(now - _ >= day)) asked.dequeue()
    failedUntil.filterInPlace { case (_, until) => until > now }

    if (pending.contains(address)) Pending
    else if (failedUntil.contains(address)) FailedLately
    else if (asked.size >= maxPerDay) {
      val first = !saidOverBudget
      saidOverBudget = true
      OverBudget(first)
    } else {
      asked.enqueue(now)
      pending += address
      saidOverBudget = false
      Ask
    }
  }

  /** The API answered with coordinates, and they are stored. */
  def resolved(address: String): Unit = synchronized { pending -= address }

  /** The API has no place for this address. */
  def unresolved(address: String): Unit = failed(address, retryUnresolvedAfter)

  /** The lookup itself failed: no answer, or an answer that is about the request, not the address. */
  def erred(address: String): Unit = failed(address, retryErrorAfter)

  private def failed(address: String, retryAfter: FiniteDuration): Unit = synchronized {
    pending -= address
    failedUntil(address) = nowMillis() + retryAfter.toMillis
  }

  /** How many addresses it holds anything about: the ones on their way and the ones that failed. */
  private[logics] def remembered: Int = synchronized { pending.size + failedUntil.size }
}

object GeocodeGate {
  sealed trait Decision
  /** Ask the API now. The asker then reports `resolved`, `unresolved` or `erred`. */
  case object Ask extends Decision
  /** Asked already, and the answer has not come. */
  case object Pending extends Decision
  /** It failed not long ago. */
  case object FailedLately extends Decision
  /** The day's questions are used up. `first` once each time the budget fills, so the log says it once. */
  final case class OverBudget(first: Boolean) extends Decision
}
