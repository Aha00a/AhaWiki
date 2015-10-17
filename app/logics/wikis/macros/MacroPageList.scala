package logics.wikis.macros

import models.DirectQuery

object MacroPageList {
  def apply() = {
    views.html.Wiki.pageList(DirectQuery.pageSelectPageList()).toString()
  }
}
