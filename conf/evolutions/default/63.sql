# Default Schema

# --- !Ups

ALTER TABLE User
    MODIFY COLUMN profileImageUrl VARCHAR(2048) NULL;

# --- !Downs

ALTER TABLE User
    MODIFY COLUMN profileImageUrl VARCHAR(512) NULL;
