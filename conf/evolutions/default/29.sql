# Default Schema

# --- !Ups
drop table UserEmail;


# --- !Downs
create table UserEmail
(
    user    int                                not null,
    email   varchar(255)                       not null,
    created datetime default CURRENT_TIMESTAMP not null,
    primary key (user, email),
    constraint UserEmail_email_uindex
        unique (email),
    constraint UserEmail_User_seq_fk
        foreign key (user) references User (seq)
);

