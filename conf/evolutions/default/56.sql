# Default Schema

# --- !Ups

CREATE TABLE UserEmail (
    user INT NOT NULL,
    email VARCHAR(255) NOT NULL,
    isPrimary BOOLEAN NOT NULL DEFAULT FALSE,
    created DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL,
    PRIMARY KEY (user, email),
    CONSTRAINT UserEmail_email_uindex UNIQUE (email),
    CONSTRAINT UserEmail_User_seq_fk FOREIGN KEY (user) REFERENCES User (seq)
);

INSERT INTO UserEmail (user, email, isPrimary)
SELECT seq, email, TRUE
FROM User;

DROP INDEX User_email_uindex ON User;

ALTER TABLE User
    DROP COLUMN email;

# --- !Downs

ALTER TABLE User
    ADD COLUMN email VARCHAR(255) NULL AFTER updated;

UPDATE User U
LEFT JOIN UserEmail UE ON UE.user = U.seq AND UE.isPrimary = TRUE
SET U.email = UE.email;

UPDATE User U
SET U.email = CONCAT('user-', U.seq, '@rollback.local')
WHERE U.email IS NULL;

ALTER TABLE User
    MODIFY COLUMN email VARCHAR(255) NOT NULL;

CREATE UNIQUE INDEX User_email_uindex ON User (email);

DROP TABLE UserEmail;
