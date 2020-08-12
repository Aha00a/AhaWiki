package logics.wikis.interpreters.ahaMark

import scala.xml.Elem

trait AhaMark {
  def toHtml: Elem 
  def toText: String = toHtml.text
}
