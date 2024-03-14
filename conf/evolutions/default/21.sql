
# --- !Ups

create index AccessLog_remoteAddress_index on AccessLog (remoteAddress);

# --- !Downs

drop index AccessLog_remoteAddress_index on AccessLog;
