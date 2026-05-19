package logics.security

object UriAttackDetector {
  private val toCheckStartsWith = Seq(
    "/wp",
    "/wordpress",
    "/new",
    "/old",
    "/backup",
    "/blog",
    "/config",
    "/.aws",
    "/.env",
    "/.git",
    "/.vscode",
  )

  //noinspection SpellCheckingInspection
  private val toCheckContains = Seq(
    "/wp-admin",
    "/wp-atom",
    "/wp-comments-post",
    "/wp-content",
    "/wp-cron",
    "/wp-feed",
    "/wp-feed-rss",
    "/wp-includes",
    "/wp-json",
    "/wp-login.php",
    "/wp-rss",
    "/wp-trackback",
    "/phpinfo.php",
    "/php_info.php",
    "/temp.php",
    "/xmlrpc.php",
  )

  // Common mass-scan signatures for PHP shell/backdoor probing.
  private val suspiciousPhpPath = "(?i)^/[a-z0-9._=-]{1,64}\\.php(?:$|[/?#]).*".r

  def isAttack(uri: String): Boolean = {
    toCheckStartsWith.exists(uri.startsWith) ||
    toCheckContains.exists(uri.contains) ||
    suspiciousPhpPath.matches(uri)
  }
}
