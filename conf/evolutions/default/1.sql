# Default Schema

# --- !Ups

CREATE TABLE Page (
    name VARCHAR(255),
    revision INTEGER,
    time BIGINT,
    author TEXT,
    remoteAddress TEXT,
    content LONGTEXT,
    comment TEXT,
    PRIMARY KEY (name,revision)
);

# --- !Downs

DROP TABLE Page;
