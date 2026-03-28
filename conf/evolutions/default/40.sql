# Default Schema

# --- !Ups

CREATE TABLE UserViewHistory (
    seq BIGINT NOT NULL AUTO_INCREMENT,
    user INT NOT NULL,
    site INT NOT NULL,
    pageName VARCHAR(255) NOT NULL,
    dateInserted DATETIME NOT NULL DEFAULT NOW(),
    CONSTRAINT UserViewHistory_pk PRIMARY KEY (seq),
    CONSTRAINT UserViewHistory_User_seq_fk FOREIGN KEY (user) REFERENCES User (seq) ON DELETE CASCADE,
    CONSTRAINT UserViewHistory_Site_seq_fk FOREIGN KEY (site) REFERENCES Site (seq) ON DELETE CASCADE
);

CREATE INDEX UserViewHistory_user_dateInserted_index ON UserViewHistory (user, dateInserted);
CREATE INDEX UserViewHistory_site_pageName_dateInserted_index ON UserViewHistory (site, pageName, dateInserted);
CREATE INDEX UserViewHistory_dateInserted_index ON UserViewHistory (dateInserted);

# --- !Downs

DROP TABLE UserViewHistory;
