# Config

# --- !Ups

CREATE TABLE Config (
	k VARCHAR(512) not null PRIMARY KEY,
	v VARCHAR(512) not null,
    created DATETIME default NOW() not null,
    updated DATETIME default NOW() not null
);

# --- !Downs

DROP TABLE Config;
