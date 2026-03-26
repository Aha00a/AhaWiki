# --- !Ups

ALTER TABLE User ADD COLUMN dateNicknameChanged DATETIME NULL AFTER nickname;

CREATE TABLE UserNicknameHistory (
    seq BIGINT NOT NULL AUTO_INCREMENT,
    user INT NOT NULL,
    old VARCHAR(255) COLLATE utf8mb4_general_ci NOT NULL,
    new VARCHAR(255) COLLATE utf8mb4_general_ci NOT NULL,
    changedBy INT NOT NULL,
    dateInserted DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,

    CONSTRAINT UserNicknameHistory_pk PRIMARY KEY (seq),
    CONSTRAINT UserNicknameHistory_User_seq_fk FOREIGN KEY (user) REFERENCES User (seq),
    CONSTRAINT UserNicknameHistory_ChangedBy_User_seq_fk FOREIGN KEY (changedBy) REFERENCES User (seq)
);

CREATE INDEX UserNicknameHistory_user_dateInserted_index ON UserNicknameHistory (user, dateInserted);


# --- !Downs

DROP TABLE UserNicknameHistory;

ALTER TABLE User DROP COLUMN dateNicknameChanged;
