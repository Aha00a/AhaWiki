# GeocodeCache

# --- !Ups

UPDATE Page SET permRead = '' WHERE permRead IS NULL;
alter table Page modify permRead varchar(255) not null;

# --- !Downs

alter table Page modify permRead varchar(255) null;
UPDATE Page SET permRead = NULL WHERE permRead = '';
