package logics

import com.aha00a.commons.utils.Using
import play.api.libs.Codecs

/**
 * The address a page gives for one of the wiki's own files under /public/: the fixed path, with a
 * digest of the file's content in the query string.
 *
 * /public/ is served with Play's default `Cache-Control: public, max-age=3600`, and until
 * 2026-09-27 the templates named each file at its bare path. A browser that had fetched a file in
 * the hour before a deploy therefore drew the new HTML with the old CSS and JS for up to an hour.
 * With the digest in the address, a file whose content changed is an address no browser has
 * cached, and a file that did not change keeps its address and its cache. The Assets controller
 * ignores the query string, so serving is unchanged. The wiki page Dev Deploying has what else
 * was considered, why it lost, and what this still leaves open.
 *
 * Each file is read once per JVM. A release is a new directory and a restart, so a file cannot
 * change under a running production instance. In dev mode it can, and the digest then lags until
 * the next reload -- harmless there, because dev mode serves assets with `no-cache`.
 *
 * A name missing from the classpath keeps its bare address: the browser gets the 404 it would have
 * got anyway, rather than every page answering 500.
 */
object PublicAsset {
  private val versions = new AhaWikiCacheMemoryTrieMap[String, Option[String]]()

  /** What goes in `v`. Its own function so a test can check an address against what is served there. */
  def version(content: Array[Byte]): String = Codecs.sha1(content).take(10)

  /** The digest of the file this instance serves at /public/`file`, or None when it has none. */
  def versionOf(file: String): Option[String] =
    versions.getOrElseUpdate(file) {
      Option(getClass.getClassLoader.getResourceAsStream(s"public/$file"))
        .map(stream => Using(stream)(stream => version(stream.readAllBytes())))
    }

  /** `file` is the path under /public/, as in `js/js.js`; `app/assets/wiki.css` is served as `wiki.css`. */
  def url(file: String): String = {
    val bare = s"/public/$file"
    versionOf(file).fold(bare)(v => s"$bare?v=$v")
  }
}
