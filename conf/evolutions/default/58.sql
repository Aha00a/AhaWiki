# Default Schema

# --- !Ups
ALTER TABLE Site ADD COLUMN publicListedOrder DECIMAL(10,2) NULL AFTER mainDomain;

# --- !Downs
ALTER TABLE Site DROP COLUMN publicListedOrder;
