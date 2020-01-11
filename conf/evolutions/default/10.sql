# GeocodeCache

# --- !Ups

alter table Page
	add permRead VARCHAR(255) null after comment;

# --- !Downs

alter table Page
    drop column permRead;
