
# --- !Ups

create table VisitLog
(
    seq int auto_increment primary key,
    site int not null,
    name varchar(255) not null,
    created datetime default CURRENT_TIMESTAMP not null,
    remoteAddress varchar(50) not null,
    referer varchar(2048) null,
    refererSite int null,
    refererName varchar(255) null,
    constraint VisitLog_Page_site_name_fk1 foreign key (site, name) references Page (site, name),
    constraint VisitLog_Page_site_name_fk2 foreign key (refererSite, refererName) references Page (site, name)
);




# --- !Downs

drop table VisitLog;


