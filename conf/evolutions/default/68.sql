# --- !Ups

# A nickname change is a request an admin reviews, not something a user does to themselves.
# `requestedNickname` takes the same case-insensitive collation as `User.nickname` so that
# "already requested" and "already taken" answer the same way the unique index does.
CREATE TABLE UserNicknameChangeRequest (
    seq BIGINT NOT NULL AUTO_INCREMENT,
    user INT NOT NULL,
    requestedNickname VARCHAR(255) COLLATE utf8mb4_general_ci NOT NULL,
    status VARCHAR(20) NOT NULL,
    requestedBy INT NOT NULL,
    reviewedBy INT NULL,
    rejectReason VARCHAR(1000) NULL,
    dateInserted DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    dateReviewed DATETIME NULL,

    CONSTRAINT UserNicknameChangeRequest_pk PRIMARY KEY (seq),
    CONSTRAINT UserNicknameChangeRequest_User_seq_fk FOREIGN KEY (user) REFERENCES User (seq),
    CONSTRAINT UserNicknameChangeRequest_RequestedBy_User_seq_fk FOREIGN KEY (requestedBy) REFERENCES User (seq),
    CONSTRAINT UserNicknameChangeRequest_ReviewedBy_User_seq_fk FOREIGN KEY (reviewedBy) REFERENCES User (seq)
);

# No partial unique index: MySQL has none, and "one pending request per user" is a rule about
# rows in one state. It is enforced in the approval transaction instead, with User.nickname's
# own unique index as the last line of defence.
CREATE INDEX UserNicknameChangeRequest_status_dateInserted_index ON UserNicknameChangeRequest (status, dateInserted);
CREATE INDEX UserNicknameChangeRequest_user_status_index ON UserNicknameChangeRequest (user, status);
CREATE INDEX UserNicknameChangeRequest_requestedNickname_status_index ON UserNicknameChangeRequest (requestedNickname, status);


# --- !Downs

DROP TABLE UserNicknameChangeRequest;
