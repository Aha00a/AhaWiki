# Default Schema

# --- !Ups

DROP INDEX CalculatedCosineSimilarity_name2_Page_site_name_fk ON CalculatedCosineSimilarity;

CREATE INDEX CalculatedCosineSimilarity_site2_name2_index
    ON CalculatedCosineSimilarity (site2, name2);

# --- !Downs

DROP INDEX CalculatedCosineSimilarity_site2_name2_index ON CalculatedCosineSimilarity;

CREATE INDEX CalculatedCosineSimilarity_name2_Page_site_name_fk
    ON CalculatedCosineSimilarity (site1, name2);
