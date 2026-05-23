# Default Schema

# --- !Ups

CREATE TABLE SiteAdmin (
    site int not null,
    user int not null,
    dateInserted DATETIME default NOW() not null,
    primary key (site, user),
    constraint SiteAdmin_Site_seq_fk foreign key (site) references Site (seq),
    constraint SiteAdmin_User_seq_fk foreign key (user) references User (seq)
);


# --- !Downs

DROP TABLE SiteAdmin;
