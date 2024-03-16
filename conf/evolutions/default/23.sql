
# --- !Ups

create table IpDeny (
    seq int auto_increment primary key,
    dateInserted datetime default CURRENT_TIMESTAMP not null,
    ip           varchar(46)                        not null
);

create index IpDeny_dateInserted_index on IpDeny (dateInserted);
create index IpDeny_ip_index on IpDeny (ip);

INSERT INTO IpDeny (ip) VALUES ('13.59.169.75');

alter table AccessLog add ipDeny int null after site;
alter table AccessLog modify status int not null after method;
alter table AccessLog modify durationMilli int not null after status;
alter table AccessLog add constraint AccessLog_IpDeny_seq_fk foreign key (ipDeny) references IpDeny (seq);


# --- !Downs
alter table AccessLog drop foreign key AccessLog_IpDeny_seq_fk;
alter table AccessLog drop column ipDeny;
drop table IpDeny;

