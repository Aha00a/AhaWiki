# Default Schema

# --- !Ups

DROP TABLE UserSite;

# --- !Downs

CREATE TABLE UserSite (
    user int NOT NULL,
    site int NOT NULL,
    created datetime DEFAULT CURRENT_TIMESTAMP NOT NULL,
    PRIMARY KEY (user, site),
    CONSTRAINT UserSite_Site_seq_fk FOREIGN KEY (site) REFERENCES Site (seq),
    CONSTRAINT UserSite_User_seq_fk FOREIGN KEY (user) REFERENCES User (seq)
);
