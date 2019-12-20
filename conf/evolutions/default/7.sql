# GeocodeCache

# --- !Ups

ALTER TABLE Page ADD dateTime DATETIME(6) NULL AFTER time;

UPDATE
    Page
    SET dateTime = DATE_ADD(from_unixtime(floor(time/1000/1000)), INTERVAL (time%1000000) MICROSECOND)
    WHERE dateTime is null;
ALTER TABLE Page MODIFY dateTime DATETIME(6) NOT NULL;

ALTER TABLE Page DROP time;

# --- !Downs

ALTER TABLE Page ADD time BIGINT NULL AFTER dateTime;

UPDATE
    Page
    SET time = UNIX_TIMESTAMP(dateTime) * 1000 * 1000
    WHERE dateTime is null;

ALTER TABLE Page MODIFY time BIGINT NOT NULL;

alter table Page DROP dateTime;
