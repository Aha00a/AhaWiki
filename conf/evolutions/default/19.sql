
# --- !Ups

create table VisitLog
(
    seq int auto_increment primary key,
    site int not null,
    name varchar(255) not null,
    created datetime default CURRENT_TIMESTAMP not null,
    remoteAddress varchar(50) not null,
    userAgent varchar(512) not null,
    referer varchar(2048) null,
    refererSite int null,
    refererName varchar(255) null,
    constraint VisitLog_Site_seq_fk foreign key (site) references Site (seq),
    constraint VisitLog_Site_seq_fk_2 foreign key (refererSite) references Site (seq)
);


# --- !Downs

drop table VisitLog;


