# --- !Ups

ALTER TABLE PageMeta
    DROP INDEX PageMeta_site_datePageLastChanged_name_index;

ALTER TABLE PageMeta
    DROP COLUMN datePageLastChanged;

# --- !Downs

ALTER TABLE PageMeta
    ADD COLUMN datePageLastChanged DATETIME NULL AFTER dateUpdated;

ALTER TABLE PageMeta
    ADD INDEX PageMeta_site_datePageLastChanged_name_index (site, datePageLastChanged, name);
