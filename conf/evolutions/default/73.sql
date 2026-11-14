# --- !Ups

# Where readers come from: one row per site, day, referring page and page landed on, counted.
# (ActorAccessLog.Referral, FilterAccessLog.referralOf, ApiAdminReport.adminReferrals.)
#
# Not a column on AccessLog: that table keeps a 10% sample of successful requests
# (AhaWiki.accessLog.sampleRate), and a referral from another site is rare enough that a sample
# would lose most of them. It is also a count, not a log -- nothing here names a reader, only the
# page that linked and the page linked to, without query strings.
CREATE TABLE Referral
(
    site      INT          NOT NULL,
    day       DATE         NOT NULL,
    source    VARCHAR(255) NOT NULL,
    landing   VARCHAR(255) NOT NULL,
    count     INT          NOT NULL DEFAULT 1,
    lastSeen  DATETIME     NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (site, day, source, landing),
    KEY Referral_day_index (day),
    CONSTRAINT Referral_Site_seq_fk FOREIGN KEY (site) REFERENCES Site (seq)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

# --- !Downs

DROP TABLE Referral;
