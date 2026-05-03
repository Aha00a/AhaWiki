# Default Schema

# --- !Ups

ALTER TABLE User
    ADD COLUMN profileImageUrl VARCHAR(512) NULL AFTER nickname;

# --- !Downs

ALTER TABLE User
    DROP COLUMN profileImageUrl;
