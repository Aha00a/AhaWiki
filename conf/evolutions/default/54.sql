# --- !Ups

ALTER TABLE Site
    ADD COLUMN abbr VARCHAR(200) NULL AFTER name;

UPDATE Site
SET abbr = name
WHERE abbr IS NULL;

ALTER TABLE Site
    MODIFY COLUMN abbr VARCHAR(200) NOT NULL;

CREATE UNIQUE INDEX Site_abbr_uindex ON Site (abbr);

ALTER TABLE Site
    ADD COLUMN mainDomain VARCHAR(255) NULL AFTER abbr;

UPDATE Site S
SET mainDomain = COALESCE((
    SELECT SD.domain
    FROM SiteDomain SD
    WHERE SD.site = S.seq
    ORDER BY SD.created ASC, SD.domain ASC
    LIMIT 1
), '');

ALTER TABLE Site
    MODIFY COLUMN mainDomain VARCHAR(255) NOT NULL DEFAULT '';

ALTER TABLE CalculatedCosineSimilarity
    DROP FOREIGN KEY CalculatedCosineSimilarity_name1_Page_site_name_fk;

ALTER TABLE CalculatedCosineSimilarity
    DROP FOREIGN KEY CalculatedCosineSimilarity_name2_Page_site_name_fk;

ALTER TABLE CalculatedCosineSimilarity
    DROP PRIMARY KEY;

ALTER TABLE CalculatedCosineSimilarity
    CHANGE COLUMN site site1 INT NOT NULL;

ALTER TABLE CalculatedCosineSimilarity
    ADD COLUMN site2 INT NULL AFTER name1;

UPDATE CalculatedCosineSimilarity
SET site2 = site1
WHERE site2 IS NULL;

ALTER TABLE CalculatedCosineSimilarity
    MODIFY COLUMN site2 INT NOT NULL;

DELETE CCS
FROM CalculatedCosineSimilarity CCS
LEFT JOIN PageMeta PM1
    ON PM1.site = CCS.site1
    AND PM1.name = CCS.name1
LEFT JOIN PageMeta PM2
    ON PM2.site = CCS.site2
    AND PM2.name = CCS.name2
WHERE PM1.name IS NULL
   OR PM2.name IS NULL;

ALTER TABLE CalculatedCosineSimilarity
    ADD PRIMARY KEY (site1, name1, site2, name2);

ALTER TABLE CalculatedCosineSimilarity
    ADD CONSTRAINT CalculatedCosineSimilarity_name1_PageMeta_site_name_fk
        FOREIGN KEY (site1, name1) REFERENCES PageMeta (site, name);

ALTER TABLE CalculatedCosineSimilarity
    ADD CONSTRAINT CalculatedCosineSimilarity_name2_PageMeta_site_name_fk
        FOREIGN KEY (site2, name2) REFERENCES PageMeta (site, name);

# --- !Downs

ALTER TABLE CalculatedCosineSimilarity
    DROP FOREIGN KEY CalculatedCosineSimilarity_name1_PageMeta_site_name_fk;

ALTER TABLE CalculatedCosineSimilarity
    DROP FOREIGN KEY CalculatedCosineSimilarity_name2_PageMeta_site_name_fk;

ALTER TABLE CalculatedCosineSimilarity
    DROP PRIMARY KEY;

DELETE FROM CalculatedCosineSimilarity
WHERE site1 != site2;

ALTER TABLE CalculatedCosineSimilarity
    DROP COLUMN site2;

ALTER TABLE CalculatedCosineSimilarity
    CHANGE COLUMN site1 site INT NOT NULL;

ALTER TABLE CalculatedCosineSimilarity
    ADD PRIMARY KEY (site, name1, name2);

ALTER TABLE CalculatedCosineSimilarity
    ADD CONSTRAINT CalculatedCosineSimilarity_name1_Page_site_name_fk
        FOREIGN KEY (site, name1) REFERENCES Page (site, name);

ALTER TABLE CalculatedCosineSimilarity
    ADD CONSTRAINT CalculatedCosineSimilarity_name2_Page_site_name_fk
        FOREIGN KEY (site, name2) REFERENCES Page (site, name);

DROP INDEX Site_abbr_uindex ON Site;

ALTER TABLE Site
    DROP COLUMN mainDomain,
    DROP COLUMN abbr;
