# PageLink

# --- !Ups

CREATE TABLE Link (
    src       VARCHAR(255),
    dst       VARCHAR(255),
    alias     VARCHAR(255),
    PRIMARY KEY (src, dst)
);


# --- !Downs

DROP TABLE Link;
