
# --- !Ups

alter table IpDeny add accessLog int null after seq;
alter table IpDeny
    add constraint IpDeny_AccessLog_seq_fk
    foreign key (accessLog) references AccessLog (seq) on delete set null;
alter table IpDeny add reason VARCHAR(255) not null;

alter table AccessLog drop foreign key AccessLog_IpDeny_seq_fk;

alter table AccessLog
    add constraint AccessLog_IpDeny_seq_fk
    foreign key (ipDeny) references IpDeny (seq) on delete set null;





# --- !Downs
alter table AccessLog drop foreign key AccessLog_IpDeny_seq_fk;
alter table AccessLog
    add constraint AccessLog_IpDeny_seq_fk
    foreign key (ipDeny) references IpDeny (seq);

alter table IpDeny drop column reason;
alter table IpDeny drop foreign key IpDeny_AccessLog_seq_fk;
alter table IpDeny drop column accessLog;

