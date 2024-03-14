
# --- !Ups

create table AccessLog
(
    seq           int auto_increment     not null primary key,
    site          int                    not null,
    dateInserted  datetime default NOW() not null,
    method        VARCHAR(16)            not null,
    path          VARCHAR(4096)          not null,
    remoteAddress VARCHAR(46)            not null,
    userAgent     VARCHAR(512)           not null,
    status        int                    not null,
    durationMilli int                    not null,
    constraint AccessLog_Site_seq_fk
        foreign key (site) references Site (seq)
);





# --- !Downs

drop table AccessLog;
