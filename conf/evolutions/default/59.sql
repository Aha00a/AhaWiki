# Default Schema

# --- !Ups

CREATE TABLE CalculatedTermFrequencyNorm (
    site INT NOT NULL,
    name VARCHAR(255) NOT NULL,
    norm DOUBLE NOT NULL,
    PRIMARY KEY (site, name),
    CONSTRAINT CalculatedTermFrequencyNorm_PageMeta_site_name_fk
        FOREIGN KEY (site, name) REFERENCES PageMeta (site, name)
        ON DELETE CASCADE
);

INSERT INTO CalculatedTermFrequencyNorm (site, name, norm)
SELECT
    CTF.site,
    CTF.name,
    SQRT(SUM(CTF.frequency * CTF.frequency)) AS norm
FROM CalculatedTermFrequency CTF
INNER JOIN PageMeta PM
    ON PM.site = CTF.site
    AND PM.name = CTF.name
GROUP BY CTF.site, CTF.name;

# --- !Downs

DROP TABLE CalculatedTermFrequencyNorm;
