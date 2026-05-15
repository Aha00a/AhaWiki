# --- !Ups
CREATE INDEX Page_site_isMinorEdit_name_revision_index ON Page (site, isMinorEdit, name, revision);

# --- !Downs
DROP INDEX Page_site_isMinorEdit_name_revision_index ON Page;
