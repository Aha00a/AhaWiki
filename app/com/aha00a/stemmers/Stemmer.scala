package com.aha00a.stemmers

import com.twitter.penguin.korean.TwitterKoreanProcessor
import com.twitter.penguin.korean.tokenizer.KoreanTokenizer.KoreanToken
import com.twitter.penguin.korean.util.KoreanPos._
import org.bitbucket.eunjeon.seunjeon.{Analyzer, Pos}
import play.api.Logger

object Stemmer {
  def stem(s: String): Seq[String] = stemSplit(s)

  private def extractEnglish(s: String): String = s.replaceAll("""[^\w\s]""", " ")
  private def extractKorean(s: String): String = s.replaceAll("""[^가-힣\s]""", " ")

  def stemEnglishPorterKoreanWith(s: String, koreanStemmer:String => Seq[String]): Seq[String] = {
    val english: String = extractEnglish(s)
    val korean = extractKorean(s)

    val englishStemmed = english.split("""\s+""").map(porterStem)
    val koreanStemmed = koreanStemmer(korean)

    englishStemmed ++ koreanStemmed
  }

  def stemSplit(s: String): Seq[String] = stemEnglishPorterKoreanWith(s, k => k.split("""\s+"""))
  def stemTwitter(s: String): Seq[String] = stemEnglishPorterKoreanWith(s, k => k.split("""(\r\n|\n)+""").flatMap(normalizeTokenizeStemFilter))
  def stemSeunjeon(s: String): Seq[String] = stemEnglishPorterKoreanWith(s, k => Analyzer.parse(k).filter(_.morpheme.poses.contains(Pos.N)).map(_.morpheme.surface))
  
  private val regexNumber = """\d+""".r

  def removeStopWord(seq:Seq[String]): Seq[String] = {
    seq.filter {
      case "" |
           "wiki" |
           "http" | "www" | "com" | "kr" |
           "the" | "to" | "of" | "for" | "is" | "a" | "in" | "and" | "be" | "or" | "i" | "you" => false
      case "것" | "수" | "일" | "를" | "에서" => false
      case regexNumber() => false
      case string if 10 < string.length => false
      case _ => true
    }
  }

  def normalizeTokenizeStemFilter(s: String): Seq[String] = {
    try {
      val n: CharSequence = TwitterKoreanProcessor.normalize(s)
      val nt: Seq[KoreanToken] = TwitterKoreanProcessor.tokenize(n)
      val nts: Seq[KoreanToken] = TwitterKoreanProcessor.stem(nt)
      val ntsf: Seq[String] = nts.filter(a => Seq(ProperNoun, Noun).contains(a.pos)).map(_.text)
      ntsf
    } catch {
      case e:Exception =>
        Logger.error(e.toString)
        Seq[String]()
    }
  }


  def porterStem(s: String): String = {
    val stemmer: PorterStemmer = new PorterStemmer
    stemmer.add(s)
    stemmer.step1()
    stemmer.step2()
    stemmer.step3()
    stemmer.step4()
    stemmer.step5a()
    stemmer.step5b()
    stemmer.b
  }


}
