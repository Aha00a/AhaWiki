# Default Schema

# --- !Ups

CREATE TABLE Page (
    name VARCHAR(255),
    revision INTEGER,
    time BIGINT,
    author TEXT,
    remoteAddress TEXT,
    content TEXT,
    comment TEXT,
    PRIMARY KEY (name,revision)
);


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
DROP TABLE Page;
