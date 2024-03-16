
# --- !Ups

DELETE FROM AccessLog WHERE 0 < seq;
alter table AccessLog add scheme VARCHAR(10) not null after method;
alter table AccessLog add host VARCHAR(255) not null after scheme;
alter table AccessLog change path uri varchar(4096) not null;
alter table AccessLog add url VARCHAR(4096) not null after uri;

# --- !Downs
alter table AccessLog drop column scheme;
alter table AccessLog drop column host;
alter table AccessLog change uri path varchar(4096) not null;
alter table AccessLog drop column url;

