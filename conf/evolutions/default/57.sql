# Default Schema

# --- !Ups

CREATE INDEX CalculatedTermFrequency_term_site_name_frequency_index
    ON CalculatedTermFrequency (term, site, name, frequency);

CREATE INDEX CalculatedCosineSimilarity_site1_name1_similarity_index
    ON CalculatedCosineSimilarity (site1, name1, similarity);

# --- !Downs

DROP INDEX CalculatedCosineSimilarity_site1_name1_similarity_index ON CalculatedCosineSimilarity;

DROP INDEX CalculatedTermFrequency_term_site_name_frequency_index ON CalculatedTermFrequency;
