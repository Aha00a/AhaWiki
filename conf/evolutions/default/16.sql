
# --- !Ups

create table User (
    seq int auto_increment,
    created DATETIME default NOW() not null,
    updated DATETIME default NOW() not null,
    id VARCHAR(32) not null,
    nickname VARCHAR(32) not null,
    constraint User_pk primary key (seq)
);

create unique index User_id_uindex on User (id);


create table Site (
    seq int auto_increment,
    created DATETIME default NOW() not null,
    updated DATETIME default NOW() not null,
    name VARCHAR(200) not null,
    constraint Site_pk primary key (seq)
);

insert into Site (name) VALUES ('');

create table UserSite (
    user int not null,
    site int not null,
    created DATETIME default NOW() not null,
    constraint UserSite_pk primary key (user, site),
    constraint UserSite_Site_seq_fk foreign key (site) references Site (seq),
    constraint UserSite_User_seq_fk foreign key (user) references User (seq)
);

create table SiteDomain(
    created DATETIME default NOW() not null,
    site int not null,
    domain VARCHAR(255) not null,
    constraint SiteDomain_pk primary key (site, domain),
    constraint SiteDomain_Site_seq_fk foreign key (site) references Site (seq)
);

create unique index SiteDomain_domain_uindex on SiteDomain (domain);

create table UserEmail(
    user int not null,
    email VARCHAR(255) not null,
    created DATETIME default NOW() not null,
    constraint UserEmail_pk primary key (user, email),
    constraint UserEmail_User_seq_fk foreign key (user) references User (seq)
);

create unique index UserEmail_email_uindex on UserEmail (email);


alter table TermFrequency drop foreign key fkTermFrequencyName;
alter table CosineSimilarity drop foreign key fkCosineSimilarityName1;
alter table CosineSimilarity drop foreign key fkCosineSimilarityName2;
alter table SchemaOrg drop foreign key SchemaOrg_Page_name_fk;

alter table Page add site int default 1 not null first;
alter table Page drop primary key, add primary key (site, name, revision);
alter table Page add constraint Page_Site_seq_fk foreign key (site) references Site (seq);
alter table Page alter column site drop default;

TRUNCATE TermFrequency;
alter table TermFrequency
    add site int default 1 not null first,
    drop primary key,
    add primary key (site, name, term),
    add constraint fkTermFrequencySiteNamePageSiteName foreign key (site, name) references Page (site, name)
;
alter table TermFrequency alter column site drop default;


TRUNCATE CosineSimilarity;
alter table CosineSimilarity
    add site int default 1 not null first,
    drop primary key,
    add primary key (site, name1, name2),
    add constraint fkCosignSimilaritySiteName1PageSiteName foreign key (site, name1) references Page (site, name),
    add constraint fkCosignSimilaritySiteName2PageSiteName foreign key (site, name2) references Page (site, name)
;
alter table CosineSimilarity alter column site drop default;


alter table Link
    add site int default 1 not null first,
    drop primary key,
    add primary key (site, src, dst, alias),
    add constraint fkLinkSite foreign key (site, src) references Page (site, name)
;
alter table Link alter column site drop default;


alter table SchemaOrg add site int default 1 not null first;
alter table SchemaOrg drop primary key, add primary key (site, page, cls, prop, value);
alter table SchemaOrg add constraint fkTermFrequencyPage foreign key (site, page) references Page (site, name);
alter table SchemaOrg alter column site drop default;


# --- !Downs

alter table SchemaOrg
    drop foreign key fkTermFrequencyPage,
    drop primary key,
    add primary key (page, cls, prop, value),
    drop column site;

alter table Link
    drop foreign key fkLinkSite,
    drop primary key,
    add primary key (src, dst, alias),
    drop column site;

TRUNCATE CosineSimilarity;
alter table CosineSimilarity
    drop foreign key fkCosignSimilaritySiteName1PageSiteName,
    drop foreign key fkCosignSimilaritySiteName2PageSiteName,
    drop primary key,
    add primary key (name1, name2),
    drop column site;

TRUNCATE TermFrequency;
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

drop table UserEmail;

drop table SiteDomain;

drop table UserSite;

drop table Site;

DROP TABLE User;



