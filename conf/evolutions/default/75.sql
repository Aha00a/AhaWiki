# --- !Ups

# A deleted page is kept, not dropped (owner's decision, 2026-11-16). Until now a delete removed
# every revision of the page and its attachments' S3 objects, and the only way back was restoring
# the whole shared database from a backup. Now the revisions move here, the attachments stay in S3
# with their rows marked Trashed, and a site admin can restore the page or delete it for good
# (PageTrashLogic). Nothing removes these rows on a schedule: the owner chose not to have the
# 30-day expiry for now.
#
# A table of its own rather than a flag on Page: Page's key is (site, name, revision), so a kept
# page would stop a new page of the same name being written, and every reader of Page would have
# to learn to skip it.
CREATE TABLE DeletedPage
(
    seq           BIGINT       NOT NULL AUTO_INCREMENT,
    site          INT          NOT NULL,
    name          VARCHAR(255) NOT NULL,
    dateDeleted   DATETIME(6)  NOT NULL,
    deletedBy     INT          DEFAULT NULL,
    viaApi        TINYINT(1)   NOT NULL DEFAULT 0,
    lastRevision  INT          NOT NULL,
    revisionCount INT          NOT NULL,
    PRIMARY KEY (seq),
    KEY DeletedPage_site_dateDeleted_index (site, dateDeleted),
    KEY DeletedPage_User_seq_fk (deletedBy),
    CONSTRAINT DeletedPage_Site_seq_fk FOREIGN KEY (site) REFERENCES Site (seq),
    CONSTRAINT DeletedPage_User_seq_fk FOREIGN KEY (deletedBy) REFERENCES `User` (seq)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

# The columns of Page, minus site and name, which DeletedPage holds once.
CREATE TABLE DeletedPageRevision
(
    deletedPage   BIGINT      NOT NULL,
    revision      INT         NOT NULL,
    dateTime      DATETIME(6) NOT NULL,
    `user`        INT         NOT NULL,
    remoteAddress TEXT        DEFAULT NULL,
    comment       TEXT        NOT NULL,
    isMinorEdit   TINYINT(1)  NOT NULL DEFAULT 0,
    viaApi        TINYINT(1)  NOT NULL DEFAULT 0,
    userApiKey    BIGINT      DEFAULT NULL,
    content       LONGTEXT    DEFAULT NULL,
    PRIMARY KEY (deletedPage, revision),
    KEY DeletedPageRevision_User_seq_fk (`user`),
    KEY DeletedPageRevision_UserApiKey_seq_fk (userApiKey),
    CONSTRAINT DeletedPageRevision_DeletedPage_seq_fk FOREIGN KEY (deletedPage) REFERENCES DeletedPage (seq) ON DELETE CASCADE,
    CONSTRAINT DeletedPageRevision_User_seq_fk FOREIGN KEY (`user`) REFERENCES `User` (seq),
    CONSTRAINT DeletedPageRevision_UserApiKey_seq_fk FOREIGN KEY (userApiKey) REFERENCES UserApiKey (seq) ON DELETE SET NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

# Trashed: the page is kept in DeletedPage and the object is still in S3. dateDeleted is set too, so
# every query that reads only live rows (dateDeleted IS NULL) already leaves these out.
ALTER TABLE Attachment
    MODIFY status ENUM('Initiated','Uploaded','Verified','Deleted','Failed','Trashed') NOT NULL;
ALTER TABLE Attachment
    ADD COLUMN deletedPage BIGINT DEFAULT NULL;
ALTER TABLE Attachment
    ADD CONSTRAINT Attachment_DeletedPage_seq_fk FOREIGN KEY (deletedPage) REFERENCES DeletedPage (seq) ON DELETE SET NULL;

# --- !Downs

# Whatever is still kept is lost with the tables. Its attachment objects stay in S3, rows Deleted.
UPDATE Attachment SET status = 'Deleted' WHERE status = 'Trashed';
ALTER TABLE Attachment DROP FOREIGN KEY Attachment_DeletedPage_seq_fk;
ALTER TABLE Attachment DROP COLUMN deletedPage;
ALTER TABLE Attachment
    MODIFY status ENUM('Initiated','Uploaded','Verified','Deleted','Failed') NOT NULL;
DROP TABLE DeletedPageRevision;
DROP TABLE DeletedPage;
