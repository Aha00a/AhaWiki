# --- !Ups

# Somebody asking for a wiki of their own. The row outlives the answer, so that how many asked
# and how many got a site can be counted. `site` is the site an approval made.
CREATE TABLE SiteRequest (
    seq BIGINT NOT NULL AUTO_INCREMENT,
    user INT NOT NULL,
    name VARCHAR(200) NOT NULL,
    subdomain VARCHAR(63) NOT NULL,
    isPublic TINYINT(1) NOT NULL,
    purpose VARCHAR(1000) NOT NULL,
    status VARCHAR(20) NOT NULL,
    reviewedBy INT NULL,
    rejectReason VARCHAR(1000) NULL,
    site INT NULL,
    dateInserted DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    dateReviewed DATETIME NULL,

    CONSTRAINT SiteRequest_pk PRIMARY KEY (seq),
    CONSTRAINT SiteRequest_User_seq_fk FOREIGN KEY (user) REFERENCES User (seq),
    CONSTRAINT SiteRequest_ReviewedBy_User_seq_fk FOREIGN KEY (reviewedBy) REFERENCES User (seq),
    CONSTRAINT SiteRequest_Site_seq_fk FOREIGN KEY (site) REFERENCES Site (seq)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

CREATE INDEX SiteRequest_status_dateInserted_index ON SiteRequest (status, dateInserted);
CREATE INDEX SiteRequest_user_status_index ON SiteRequest (user, status);
CREATE INDEX SiteRequest_subdomain_status_index ON SiteRequest (subdomain, status);

# --- !Downs

DROP TABLE SiteRequest;
