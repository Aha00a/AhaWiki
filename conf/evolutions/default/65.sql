# Default Schema

# --- !Ups

CREATE TABLE UserApiKey (
    seq BIGINT NOT NULL AUTO_INCREMENT,
    `user` INT NOT NULL,
    keyHash VARCHAR(64) NOT NULL,
    keyPrefix VARCHAR(32) NOT NULL,
    label VARCHAR(255) NOT NULL,
    dateInserted DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    dateLastUsed DATETIME NULL,
    dateRevoked DATETIME NULL,
    CONSTRAINT UserApiKey_pk PRIMARY KEY (seq),
    CONSTRAINT UserApiKey_keyHash_uindex UNIQUE (keyHash),
    CONSTRAINT UserApiKey_User_seq_fk FOREIGN KEY (`user`) REFERENCES User (seq)
);

CREATE INDEX UserApiKey_user_dateRevoked_index
    ON UserApiKey (`user`, dateRevoked);

ALTER TABLE Page
    ADD COLUMN viaApi BOOLEAN NOT NULL DEFAULT FALSE AFTER isMinorEdit;

# --- !Downs

ALTER TABLE Page
    DROP COLUMN viaApi;

DROP TABLE UserApiKey;
