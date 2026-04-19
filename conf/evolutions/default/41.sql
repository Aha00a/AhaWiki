# Default Schema

# --- !Ups
ALTER TABLE Page
    ADD COLUMN isMinorEdit BOOLEAN NOT NULL DEFAULT FALSE AFTER permRead;


# --- !Downs
ALTER TABLE Page
    DROP COLUMN isMinorEdit;
