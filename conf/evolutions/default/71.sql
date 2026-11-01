# --- !Ups

# Recent changes (Api.change behind the RecentChanges macro, ApiV1.changes) order one site's
# revisions by time and take the first rows. No key on Page led with (site, dateTime), so each
# call read every revision of the site and sorted them - and the macro loads itself on every
# view of the page that carries it. The queries order by dateTime, revision and name, all
# descending, so this key is read backward and the scan stops at the LIMIT. isMinorEdit and
# viaApi follow so the filter on them is answered from the key as well.
CREATE INDEX Page_site_dateTime_revision_name_isMinorEdit_viaApi_index
    ON Page (site, dateTime, revision, name, isMinorEdit, viaApi);

# Twin pages ask which sites hold a page of one name (PageMeta.selectSiteSeqsHavingPage). Every
# other key of PageMeta leads with site, so the question had no key at all - the view used to
# answer it from each site's cached page list instead, one Redis round trip per site.
CREATE INDEX PageMeta_name_site_index
    ON PageMeta (name, site);

# --- !Downs

DROP INDEX PageMeta_name_site_index ON PageMeta;
DROP INDEX Page_site_dateTime_revision_name_isMinorEdit_viaApi_index ON Page;
