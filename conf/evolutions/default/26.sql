# --- !Ups
CREATE INDEX AccessLog_dateInserted_index ON AccessLog (dateInserted);

# --- !Downs
DROP INDEX AccessLog_dateInserted_index ON AccessLog;
