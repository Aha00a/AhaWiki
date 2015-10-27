# PageLink

# --- !Ups

ALTER TABLE TermFrequency ADD CONSTRAINT fkTermFrequencyName FOREIGN KEY (name) REFERENCES Page(name);
ALTER TABLE CosineSimilarity ADD CONSTRAINT fkCosineSimilarityName1 FOREIGN KEY (name1) REFERENCES Page(name);
ALTER TABLE CosineSimilarity ADD CONSTRAINT fkCosineSimilarityName2 FOREIGN KEY (name2) REFERENCES Page(name);


# --- !Downs

ALTER TABLE TermFrequency DROP FOREIGN KEY fkTermFrequencyName;
ALTER TABLE CosineSimilarity DROP FOREIGN KEY fkCosineSimilarityName1;
ALTER TABLE CosineSimilarity DROP FOREIGN KEY fkCosineSimilarityName2;
