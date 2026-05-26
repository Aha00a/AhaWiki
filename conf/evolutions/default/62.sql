# Default Schema

# --- !Ups

CREATE INDEX Page_site_name_revision_dateTime_user_index ON Page (site, name, revision, dateTime, user);


# --- !Downs

DROP INDEX Page_site_name_revision_dateTime_user_index ON Page;
