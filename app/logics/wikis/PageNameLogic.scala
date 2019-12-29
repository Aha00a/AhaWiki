package logics.wikis

object PageNameLogic {
  def isExternal(s: String): Boolean = s.contains("://")

}
