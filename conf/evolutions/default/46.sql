# --- !Ups

ALTER TABLE AccessLog
    DROP COLUMN url;

# --- !Downs

ALTER TABLE AccessLog
    ADD COLUMN url VARCHAR(4096) NULL AFTER uri;
