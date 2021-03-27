
# --- !Ups

create table Referer
(
    seq int auto_increment primary key,
    site int not null,
    name varchar(255) not null,
    created datetime default CURRENT_TIMESTAMP not null,
    url varchar(2048) not null,
    remoteAddress varchar(50) not null,
    constraint Referer_Page_site_name_fk foreign key (site, name) references Page (site, name)
);




# --- !Downs

drop table Referer;


