# --- !Ups
CREATE TABLE CalculatedTerm (
    seq INT AUTO_INCREMENT PRIMARY KEY,
    dateInserted DATETIME DEFAULT NOW() NOT NULL,
    term VARCHAR(255) NOT NULL UNIQUE
);
INSERT INTO CalculatedTerm (term) VALUES ('aha00a');

CREATE TABLE CalculatedTermFrequency (
    site INT NOT NULL,
    name VARCHAR(255) NOT NULL,
    term INT NOT NULL,
    frequency INT NOT NULL,
    primary key (site, name, term),
    CONSTRAINT CalculatedTermFrequency_Page_site_name_fk FOREIGN KEY (site, name) REFERENCES Page (site, name),
    CONSTRAINT CalculatedTermFrequency_CalculatedTerm_seq_fk FOREIGN KEY (term) REFERENCES CalculatedTerm (seq)
);

CREATE TABLE CalculatedCosineSimilarity
(
    site       INT                     NOT NULL,
    name1      VARCHAR(255) DEFAULT '' NOT NULL,
    name2      VARCHAR(255) DEFAULT '' NOT NULL,
    similarity DOUBLE                  NULL,
    PRIMARY KEY (site, name1, name2),
    constraint CalculatedCosineSimilarity_name1_Page_site_name_fk FOREIGN KEY (site, name1) REFERENCES Page (site, name),
    constraint CalculatedCosineSimilarity_name2_Page_site_name_fk FOREIGN KEY (site, name2) REFERENCES Page (site, name)
);


# --- !Downs
DROP TABLE CalculatedCosineSimilarity;
DROP TABLE CalculatedTermFrequency;
DROP TABLE CalculatedTerm;

