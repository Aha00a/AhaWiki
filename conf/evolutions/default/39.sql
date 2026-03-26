# Default Schema

# --- !Ups

ALTER TABLE CalculatedTermFrequency
    DROP FOREIGN KEY CalculatedTermFrequency_CalculatedTerm_seq_fk;

ALTER TABLE CalculatedTerm
    MODIFY COLUMN seq BIGINT NOT NULL AUTO_INCREMENT;

ALTER TABLE CalculatedTermFrequency
    MODIFY COLUMN term BIGINT NOT NULL;

ALTER TABLE CalculatedTermFrequency
    ADD CONSTRAINT CalculatedTermFrequency_CalculatedTerm_seq_fk
        FOREIGN KEY (term) REFERENCES CalculatedTerm (seq);

# --- !Downs

ALTER TABLE CalculatedTermFrequency
    DROP FOREIGN KEY CalculatedTermFrequency_CalculatedTerm_seq_fk;

ALTER TABLE CalculatedTermFrequency
    MODIFY COLUMN term INT NOT NULL;

ALTER TABLE CalculatedTerm
    MODIFY COLUMN seq INT NOT NULL AUTO_INCREMENT;

ALTER TABLE CalculatedTermFrequency
    ADD CONSTRAINT CalculatedTermFrequency_CalculatedTerm_seq_fk
        FOREIGN KEY (term) REFERENCES CalculatedTerm (seq);
