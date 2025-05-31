# Default Schema

# --- !Ups

CREATE VIEW SitePageNameRevision AS
SELECT site, name, MAX(revision) AS revision FROM Page
GROUP BY site, name;


# --- !Downs

DROP VIEW SitePageNameRevision;
