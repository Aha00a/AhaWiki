package logics.wikis.macros

import models.ContextWikiPage

object MacroRecentChanges extends TraitMacro {

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    s"""
       |<div class="macro-recent-changes" data-include-minor-edit="0">
       |  <div class="macro-recent-changes-status">Loading recent changes...</div>
       |  <table class="macro-recent-changes-table" style="display:none;">
       |    <thead>
       |      <tr>
       |        <th>Name</th>
       |        <th>Revision</th>
       |        <th>At</th>
       |        <th>By</th>
       |        <th>Comment</th>
       |      </tr>
       |    </thead>
       |    <tbody></tbody>
       |  </table>
       |</div>
       |<script>
       |(function () {
       |  function esc(v) {
       |    return String(v == null ? "" : v).replace(/[&<>"']/g, function (c) {
       |      return {'&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;'}[c];
       |    });
       |  }
       |
       |  function render(container, rows) {
       |    var status = container.querySelector('.macro-recent-changes-status');
       |    var table = container.querySelector('.macro-recent-changes-table');
       |    var tbody = table.querySelector('tbody');
       |    tbody.innerHTML = rows.map(function (row) {
       |      var comment = (row.isMinorEdit ? "[minor] " : "") + (row.comment || "");
       |      var author = row.nickname ? row.nickname : row.remoteAddressMasked;
       |      return "<tr>" +
       |        "<td><a href='/w/" + encodeURIComponent(row.name) + "'>" + esc(row.name) + "</a></td>" +
       |        "<td><a rel='nofollow' href='/w/" + encodeURIComponent(row.name) + "?action=diff&after=" + encodeURIComponent(row.revision) + "'>" + esc(row.revision) + "</a></td>" +
       |        "<td>" + esc(row.dateTime) + "</td>" +
       |        "<td>" + esc(author) + "</td>" +
       |        "<td>" + esc(comment) + "</td>" +
       |        "</tr>";
       |    }).join("");
       |
       |    status.style.display = "none";
       |    table.style.display = "";
       |    if (window.jQuery && window.jQuery.fn && window.jQuery.fn.tablesorter) {
       |      window.jQuery(table).tablesorter();
       |    }
       |  }
       |
       |  function load(container) {
       |    var includeMinorEdit = container.getAttribute("data-include-minor-edit") === "1" ? "1" : "0";
       |    var status = container.querySelector('.macro-recent-changes-status');
       |    fetch("/api/change?includeMinorEdit=" + includeMinorEdit, { credentials: "same-origin" })
       |      .then(function (res) {
       |        if (!res.ok) throw new Error("HTTP " + res.status);
       |        return res.json();
       |      })
       |      .then(function (rows) { render(container, rows || []); })
       |      .catch(function (e) {
       |        status.textContent = "Failed to load recent changes. " + e;
       |      });
       |  }
       |
       |  var containers = document.querySelectorAll(".macro-recent-changes");
       |  for (var i = 0; i < containers.length; i++) load(containers[i]);
       |})();
       |</script>
       |""".stripMargin
  }
}
