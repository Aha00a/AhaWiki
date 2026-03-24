# Default Schema

# --- !Ups

CREATE TABLE Attachment (
    seq BIGINT AUTO_INCREMENT,
    dateInserted DATETIME DEFAULT NOW() NOT NULL,
    dateUpdated DATETIME DEFAULT NOW() NOT NULL,
    dateUploaded DATETIME NULL,
    dateDeleted DATETIME NULL,
    site INT NOT NULL,
    pageName VARCHAR(255) NOT NULL,
    user INT NULL,
    uploaderEmail VARCHAR(255) NULL,
    originalFilename VARCHAR(255) NOT NULL,
    storedFilename VARCHAR(255) NOT NULL,
    bucket VARCHAR(255) NOT NULL,
    objectKey VARCHAR(512) NOT NULL,
    contentType VARCHAR(255) NOT NULL,
    fileSize BIGINT NOT NULL,
    sha256 CHAR(64) NULL,
    etag VARCHAR(255) NULL,
    status ENUM('Initiated', 'Uploaded', 'Verified', 'Deleted', 'Failed') NOT NULL,
    imageWidth INT NULL,
    imageHeight INT NULL,
    CONSTRAINT Attachment_pk PRIMARY KEY (seq),
    CONSTRAINT Attachment_Site_seq_fk FOREIGN KEY (site) REFERENCES Site (seq),
    CONSTRAINT Attachment_User_seq_fk FOREIGN KEY (user) REFERENCES User (seq)
);

CREATE UNIQUE INDEX Attachment_objectKey_uindex ON Attachment (objectKey);
CREATE INDEX Attachment_site_pageName_dateInserted_index ON Attachment (site, pageName, dateInserted);
CREATE INDEX Attachment_site_status_dateInserted_index ON Attachment (site, status, dateInserted);
CREATE INDEX Attachment_user_dateInserted_index ON Attachment (user, dateInserted);
CREATE INDEX Attachment_dateDeleted_index ON Attachment (dateDeleted);

# --- !Downs

DROP TABLE Attachment;
