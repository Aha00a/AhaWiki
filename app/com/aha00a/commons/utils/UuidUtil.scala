package com.aha00a.commons.utils

object UuidUtil {
  // Every id a template draws fresh on each render uses this form, and scripts/compare-instances.sh
  // masks exactly this form when it compares two instances page by page. InterpreterMap used a
  // dashless variant until 2026-10-03, so every page with a map differed between two renders of
  // the same code and was set aside by every canary comparison -- never compared at all.
  def newString: String = java.util.UUID.randomUUID.toString
}
