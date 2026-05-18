# --- !Ups
CREATE TABLE PageMeta (
  site INT NOT NULL,
  name VARCHAR(255) NOT NULL,
  dateInserted DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
  dateUpdated DATETIME NULL,
  datePageLastChanged DATETIME NULL,
  revision INTEGER NOT NULL,
  image VARCHAR(512) NULL,
  permRead VARCHAR(255) NOT NULL DEFAULT '',
  size BIGINT NOT NULL DEFAULT 0,
  PRIMARY KEY (site, name),
  CONSTRAINT PageMeta_Page_site_name_revision_fk FOREIGN KEY (site, name, revision) REFERENCES Page (site, name, revision),
  INDEX PageMeta_site_dateUpdated_name_index (site, dateUpdated, name),
  INDEX PageMeta_site_datePageLastChanged_name_index (site, datePageLastChanged, name)
);

# --- !Downs
DROP TABLE PageMeta;
