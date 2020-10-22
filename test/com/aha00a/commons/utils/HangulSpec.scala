package com.aha00a.commons.utils

import org.scalatest.freespec.AnyFreeSpec

class HangulSpec extends AnyFreeSpec {
  "firstLetterForIndex" - {
    import com.aha00a.commons.utils.LetterUtil.firstLetterForIndex

    "roman" in {
      assert(firstLetterForIndex("A") === 'A')
      assert(firstLetterForIndex("a") === 'A')
      assert(firstLetterForIndex("Abc") === 'A')
      assert(firstLetterForIndex("abc") === 'A')

      assert(firstLetterForIndex("Ā") === 'A')
      assert(firstLetterForIndex("ȧ") === 'A')
      assert(firstLetterForIndex("ǒ") === 'O')

      assert(
        "ÈÉÊËÛÙÏÎÀÂÔèéêëûùïîàâôÇçÃãÕõçÇáéíóúýÁÉÍÓÚÝàèìòùÀÈÌÒÙãõñäëïöüÿÄËÏÖÜÃÕÑâêîôûÂÊÎÔÛ".map(c => firstLetterForIndex(c + "postfix")).mkString ===
        "EEEEUUIIAAOeeeeuuiiaaoCcAaOocCaeiouyAEIOUYaeiouAEIOUaonaeiouyAEIOUAONaeiouAEIOU".toUpperCase()
      )
    }

    "ko" in {
      assert(firstLetterForIndex("김") === '가')
      assert(firstLetterForIndex("아") === '아')
      assert(firstLetterForIndex("하") === '하')

      assert(firstLetterForIndex("홍") === '하')
      assert(firstLetterForIndex("길") === '가')
      assert(firstLetterForIndex("동") === '다')
    }

    "zh" in {
      assert(firstLetterForIndex("金") === '金')
      assert(firstLetterForIndex("我") === '我')
      assert(firstLetterForIndex("下") === '下')
    }

    "ja" in {
      assert(firstLetterForIndex("金") === '金')
      assert(firstLetterForIndex("ア") === 'ア')
      assert(firstLetterForIndex("ハ") === 'ハ')
    }
  }
}
