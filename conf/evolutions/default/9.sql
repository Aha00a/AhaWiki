# GeocodeCache

# --- !Ups

alter table Page modify comment text not null after remoteAddress;


# --- !Downs

alter table Page modify content longtext null after remoteAddress;

