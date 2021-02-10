
# --- !Ups

create table User (
    seq int auto_increment,
    id VARCHAR(32) not null,
    nickname VARCHAR(32) not null,
    constraint User_pk primary key (seq)
);

create unique index User_id_uindex on User (id);


create table Site (
    seq int auto_increment,
    domain VARCHAR(128) not null,
    constraint Site_pk
        primary key (seq)
);

create unique index Site_domain_uindex on Site (domain);

insert into Site (domain) VALUES ('');


create table UserSite (
    user int not null,
    site int not null,
    constraint UserSite_pk primary key (user, site),
    constraint UserSite_Site_seq_fk foreign key (site) references Site (seq),
    constraint UserSite_User_seq_fk foreign key (user) references User (seq)
);


alter table TermFrequency drop foreign key fkTermFrequencyName;
alter table CosineSimilarity drop foreign key fkCosineSimilarityName1;
alter table CosineSimilarity drop foreign key fkCosineSimilarityName2;
alter table SchemaOrg drop foreign key SchemaOrg_Page_name_fk;

alter table Page add site int default 1 not null first;
alter table Page drop primary key, add primary key (site, name, revision);
alter table Page add constraint Page_Site_seq_fk foreign key (site) references Site (seq);

alter table TermFrequency
    add site int default 1 not null first,
    drop primary key,
    add primary key (site, name, term),
    add constraint fkTermFrequencySiteNamePageSiteName foreign key (site, name) references Page (site, name)
;

TRUNCATE CosineSimilarity;
alter table CosineSimilarity
    add site int default 1 not null first,
    drop primary key,
    add primary key (site, name1, name2),
    add constraint fkCosignSimilaritySiteName1PageSiteName foreign key (site, name1) references Page (site, name),
    add constraint fkCosignSimilaritySiteName2PageSiteName foreign key (site, name2) references Page (site, name)
;

alter table SchemaOrg add site int default 1 not null first;
alter table SchemaOrg drop primary key, add primary key (site, page, cls, prop, value);
alter table SchemaOrg add constraint fkTermFrequencyPage foreign key (site, page) references Page (site, name);


# --- !Downs

alter table SchemaOrg
    drop foreign key fkTermFrequencyPage,
    drop primary key,
    add primary key (page, cls, prop, value),
    drop column site;

alter table CosineSimilarity
    drop foreign key fkCosignSimilaritySiteName1PageSiteName,
    drop foreign key fkCosignSimilaritySiteName2PageSiteName,
    drop primary key,
    add primary key (name1, name2),
    drop column site;

alter table TermFrequency
    drop foreign key fkTermFrequencySiteNamePageSiteName,
    drop primary key,
    add primary key (name, term),
    drop column site;

alter table Page drop foreign key Page_Site_seq_fk;
alter table Page drop primary key, add primary key (name, revision);
alter table Page drop column site;

alter table SchemaOrg add constraint SchemaOrg_Page_name_fk foreign key (page) references Page (name) on update cascade on delete cascade;
ALTER TABLE CosineSimilarity ADD CONSTRAINT fkCosineSimilarityName1 FOREIGN KEY (name1) REFERENCES Page(name);
ALTER TABLE CosineSimilarity ADD CONSTRAINT fkCosineSimilarityName2 FOREIGN KEY (name2) REFERENCES Page(name);
ALTER TABLE TermFrequency ADD CONSTRAINT fkTermFrequencyName FOREIGN KEY (name) REFERENCES Page(name);

drop table UserSite;

drop table Site;

DROP TABLE User;



