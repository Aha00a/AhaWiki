
# --- !Ups

alter table Config
    add site int default 1 not null first,
    drop primary key,
    add primary key (site, k),
    add constraint fkConfigSiteSiteSeq foreign key (site) references Site (seq)
;
alter table Config alter column site drop default;

# --- !Downs

alter table Config
    drop foreign key fkConfigSiteSiteSeq,
    drop primary key,
    add primary key (k),
    drop column site;

