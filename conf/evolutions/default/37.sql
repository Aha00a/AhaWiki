# Default Schema

# --- !Ups

DROP VIEW IF EXISTS SitePageNameRevision;

# --- !Downs

CREATE VIEW SitePageNameRevision AS
SELECT site, name, MAX(revision) AS revision
FROM Page
GROUP BY site, name;
