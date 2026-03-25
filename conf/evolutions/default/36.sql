# Default Schema

# --- !Ups
ALTER TABLE Page
    DROP COLUMN author;


# --- !Downs
ALTER TABLE Page
    ADD COLUMN author TEXT NULL AFTER dateTime;
