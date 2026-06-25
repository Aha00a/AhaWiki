# --- !Ups

ALTER TABLE PageMeta
    ADD COLUMN description VARCHAR(512) NULL AFTER image;

# --- !Downs

ALTER TABLE PageMeta
    DROP COLUMN description;
