package logics.security

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ConcurrentHashMap.newKeySet
import javax.inject.Singleton
import scala.util.Random

// Detects two bot patterns for a given IP within the sliding window:
//   1. High rate  : too many /w/* requests per second
//   2. Page-only  : many /w/* requests with almost no human signals
//      (real browsers always mix in /public, /assets, /api/links, /search, etc.)
//
// Also maintains knownBanned — an in-memory set of banned IPs.
// FilterAccessLog uses this to skip the IpDeny DB query for already-known bad IPs.
@Singleton
class IpRateLimiter {
  import IpRateLimiter._

  private val windowMs: Long = 30_000L

  // pattern 1 – pure speed (e.g. 4-5 req/sec)
  private val rateThreshold: Int = 30

  // pattern 2 – page-only scraper
  private val botPageMin: Int        = 5
  private val botHumanSignalMin: Int = 3

  private case class IpWindow(
    wikiPages:    java.util.ArrayDeque[Long],
    humanSignals: java.util.ArrayDeque[Long],
  )

  private val windows     = new ConcurrentHashMap[String, IpWindow]()
  private val knownBanned = newKeySet[String]()
  private val knownClean  = new ConcurrentHashMap[String, Long]()  // ip → expireAtMs
  private val cleanTtlMs: Long = 60_000L

  def isKnownBanned(ip: String): Boolean = knownBanned.contains(ip)

  def isKnownClean(ip: String): Boolean = {
    val exp = knownClean.getOrDefault(ip, 0L)
    if (exp > System.currentTimeMillis()) true
    else { knownClean.remove(ip); false }
  }

  def markClean(ip: String): Unit =
    knownClean.put(ip, System.currentTimeMillis() + cleanTtlMs)

  def ban(ip: String): Unit = {
    knownBanned.add(ip)
    knownClean.remove(ip)
  }

  def unban(ip: String): Unit = {
    knownBanned.remove(ip)
    windows.remove(ip)
    knownClean.remove(ip)
  }

  def recordAndCheck(ip: String, uri: String): Boolean = {
    if (knownBanned.contains(ip)) return true

    if (Random.nextInt(1000) == 0) cleanup()

    val cat    = categorize(uri)
    val now    = System.currentTimeMillis()
    val cutoff = now - windowMs

    val w = windows.computeIfAbsent(ip, _ => IpWindow(
      new java.util.ArrayDeque[Long](),
      new java.util.ArrayDeque[Long](),
    ))

    w.synchronized {
      pruneOld(w.wikiPages,    cutoff)
      pruneOld(w.humanSignals, cutoff)

      cat match {
        case WikiPage    => w.wikiPages.addLast(now)
        case HumanSignal => w.humanSignals.addLast(now)
      }

      val pages  = w.wikiPages.size()
      val humans = w.humanSignals.size()

      val detected = (pages >= rateThreshold) || (pages >= botPageMin && humans < botHumanSignalMin)
      if (detected) ban(ip)
      detected
    }
  }

  def cleanup(): Unit = {
    val now    = System.currentTimeMillis()
    val cutoff = now - windowMs
    val iter   = windows.entrySet().iterator()
    while (iter.hasNext) {
      val e = iter.next()
      e.getValue.synchronized {
        pruneOld(e.getValue.wikiPages,    cutoff)
        pruneOld(e.getValue.humanSignals, cutoff)
        if (e.getValue.wikiPages.isEmpty && e.getValue.humanSignals.isEmpty) iter.remove()
      }
    }
    val cleanIter = knownClean.entrySet().iterator()
    while (cleanIter.hasNext) {
      if (cleanIter.next().getValue <= now) cleanIter.remove()
    }
  }

  private def pruneOld(deque: java.util.ArrayDeque[Long], cutoff: Long): Unit =
    while (!deque.isEmpty && deque.peekFirst() < cutoff) deque.pollFirst()
}

object IpRateLimiter {
  sealed trait UriCategory
  case object WikiPage    extends UriCategory
  case object HumanSignal extends UriCategory

  def categorize(uri: String): UriCategory =
    if (uri.startsWith("/w/")) WikiPage
    else                       HumanSignal
}
