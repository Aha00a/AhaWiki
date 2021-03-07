
# --- !Ups

create table Permission
(
    site int not null,
    target varchar(255) not null,
    actor varchar(255) not null,
    action int not null,
    created DATETIME default NOW() not null,
    primary key (site, target, actor),
    constraint Permission_Site_seq_fk foreign key (site) references Site (seq)
);



# --- !Downs

drop table Permission;


