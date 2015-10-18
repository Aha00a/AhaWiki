# Default Schema

# --- !Ups

CREATE TABLE TermFrequency (
    name       VARCHAR(255),
    term       VARCHAR(255),
    frequency  INTEGER,
    PRIMARY KEY (name, term)
);

CREATE TABLE CosineSimilarity (
    name1 VARCHAR(255),
    name2 VARCHAR(255),
    similarity DOUBLE,
    PRIMARY KEY (name1, name2)
);

# --- !Downs

DROP TABLE CosineSimilarity;
DROP TABLE TermFrequency;
