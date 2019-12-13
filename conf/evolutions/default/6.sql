# GeocodeCache

# --- !Ups

UPDATE Page
    SET comment = ''
    WHERE comment IS NULL;

alter table Page modify comment text not null;

# --- !Downs

alter table Page modify comment text null;
