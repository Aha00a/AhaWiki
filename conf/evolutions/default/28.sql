# Default Schema

# --- !Ups

DROP TABLE CosineSimilarity;
DROP TABLE TermFrequency;


# --- !Downs

create table TermFrequency
(
    site      int                     not null,
    name      varchar(255) default '' not null,
    term      varchar(255) default '' not null,
    frequency int                     null,
    primary key (site, name, term),
    constraint fkTermFrequencySiteNamePageSiteName
        foreign key (site, name) references Page (site, name)
);

create table CosineSimilarity
(
    site       int                     not null,
    name1      varchar(255) default '' not null,
    name2      varchar(255) default '' not null,
    similarity double                  null,
    primary key (site, name1, name2),
    constraint fkCosignSimilaritySiteName1PageSiteName
        foreign key (site, name1) references Page (site, name),
    constraint fkCosignSimilaritySiteName2PageSiteName
        foreign key (site, name2) references Page (site, name)
);

