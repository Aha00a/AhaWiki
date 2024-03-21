# --- !Ups
alter table AccessLog add user int null after site;
alter table AccessLog add constraint AccessLog_User_seq_fk foreign key (user) references User (seq);

# --- !Downs
alter table AccessLog drop foreign key AccessLog_User_seq_fk;
alter table AccessLog drop column user;

