package com.aha00a.commons.utils

import java.security.SecureRandom

object NanoIdUtil {
  private val Alphabet: IndexedSeq[Char] = "0123456789abcdefghjkmnpqrtuvwxyz".toIndexedSeq
  private val Random = new SecureRandom()

  def newId(length: Int = 16): String = {
    require(length > 0, "length must be > 0")
    val sb = new StringBuilder(length)
    for (_ <- 0 until length) {
      sb.append(Alphabet(Random.nextInt(Alphabet.length)))
    }
    sb.toString()
  }
}
