# Default Schema

# --- !Ups
alter table Page
    add user int null after author;

alter table Page
    add constraint Page_User_seq_fk
        foreign key (user) references User (seq);

UPDATE Page
    INNER JOIN User ON Page.author = User.email
    SET Page.user = User.seq
    WHERE Page.user IS NULL;

alter table Page
    modify user int not null;

# --- !Downs
alter table Page
    drop foreign key Page_User_seq_fk;

alter table Page
    drop column user;
