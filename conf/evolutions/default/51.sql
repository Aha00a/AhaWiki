# --- !Ups

ALTER TABLE Page
    DROP COLUMN permRead;

ALTER TABLE PageMeta
    DROP COLUMN permRead;


# --- !Downs

ALTER TABLE Page
    ADD COLUMN permRead VARCHAR(255) NOT NULL DEFAULT '' AFTER comment;

ALTER TABLE PageMeta
    ADD COLUMN permRead VARCHAR(255) NOT NULL DEFAULT '' AFTER image;
