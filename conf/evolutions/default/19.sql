
# --- !Ups


alter table User drop column id;
alter table User drop column nickname;

alter table User add email varchar(255) not null;
create unique index User_email_uindex on User (email);





# --- !Downs
alter table User drop column email;

alter table User add column id VARCHAR(32) not null;
alter table User add column nickname VARCHAR(32) not null;
create unique index User_id_uindex on User (id);

