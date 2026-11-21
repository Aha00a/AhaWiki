package logics

import logics.GeocodeGate.{Ask, FailedLately, OverBudget, Pending}
import org.scalatest.freespec.AnyFreeSpec

import scala.concurrent.duration._

/**
 * Which addresses are asked of the Geocoding API, and when. The clock is the spec's own, so a day
 * passes when the spec says so.
 */
class GeocodeGateSpec extends AnyFreeSpec {
  private class Clock { var now: Long = 1000L; def pass(d: FiniteDuration): Unit = now += d.toMillis }
  private def gateOf(clock: Clock, maxPerDay: Int = 3): GeocodeGate =
    new GeocodeGate(maxPerDay, retryUnresolvedAfter = 1.day, retryErrorAfter = 10.minutes, nowMillis = () => clock.now)

  "asks for an address once while its answer is on the way" in {
    val gate = gateOf(new Clock)
    assert(gate.decide("Seoul") === Ask)
    assert(gate.decide("Seoul") === Pending)
    assert(gate.decide("Seoul") === Pending)
  }

  "keeps nothing about an address once it is resolved: the stored coordinates answer from then on" in {
    val gate = gateOf(new Clock)
    assert(gate.decide("Seoul") === Ask)
    gate.resolved("Seoul")
    assert(gate.remembered === 0)
    assert(gate.decide("Seoul") === Ask)
  }

  "leaves an address Google has no place for alone for a day" in {
    val clock = new Clock
    val gate = gateOf(clock)
    assert(gate.decide("nowhere at all") === Ask)
    gate.unresolved("nowhere at all")
    assert(gate.decide("nowhere at all") === FailedLately)
    clock.pass(23.hours)
    assert(gate.decide("nowhere at all") === FailedLately)
    clock.pass(2.hours)
    assert(gate.decide("nowhere at all") === Ask)
  }

  "tries again much sooner when it was the lookup that failed, not the address" in {
    val clock = new Clock
    val gate = gateOf(clock)
    assert(gate.decide("Seoul") === Ask)
    gate.erred("Seoul")
    assert(gate.decide("Seoul") === FailedLately)
    clock.pass(11.minutes)
    assert(gate.decide("Seoul") === Ask)
  }

  "stops at the day's budget, and marks the first refusal so the log can say it once" in {
    val gate = gateOf(new Clock, maxPerDay = 3)
    Seq("a", "b", "c").foreach(address => assert(gate.decide(address) === Ask))
    assert(gate.decide("d") === OverBudget(first = true))
    assert(gate.decide("e") === OverBudget(first = false))
    assert(gate.decide("d") === OverBudget(first = false))
  }

  "counts a lookup against the budget whether it was answered or not" in {
    val gate = gateOf(new Clock, maxPerDay = 2)
    assert(gate.decide("a") === Ask)
    gate.resolved("a")
    assert(gate.decide("b") === Ask)
    gate.unresolved("b")
    assert(gate.decide("c") === OverBudget(first = true))
  }

  "gives a place back a day after it was used, and says so again the next time it fills" in {
    val clock = new Clock
    val gate = gateOf(clock, maxPerDay = 2)
    assert(gate.decide("a") === Ask)
    clock.pass(1.hour)
    assert(gate.decide("b") === Ask)
    assert(gate.decide("c") === OverBudget(first = true))
    clock.pass(23.hours + 1.minute)
    assert(gate.decide("c") === Ask)
    assert(gate.decide("d") === OverBudget(first = true))
    clock.pass(1.hour)
    assert(gate.decide("d") === Ask)
  }

  "remembers nothing about an address it turned away, however many there are" in {
    val gate = gateOf(new Clock, maxPerDay = 2)
    assert(gate.decide("a") === Ask)
    assert(gate.decide("b") === Ask)
    (1 to 50000).foreach(i => assert(gate.decide(s"made up $i").isInstanceOf[OverBudget]))
    // The two that were let through and are still on their way; none of the fifty thousand.
    assert(gate.remembered === 2)
  }

  "forgets a failure when its time is up, so what it holds does not grow with the days" in {
    val clock = new Clock
    val gate = gateOf(clock, maxPerDay = 3)
    Seq("x", "y", "z").foreach { address => assert(gate.decide(address) === Ask); gate.unresolved(address) }
    assert(gate.remembered === 3)
    clock.pass(25.hours)
    assert(gate.decide("new") === Ask)
    assert(gate.remembered === 1)
  }
}
