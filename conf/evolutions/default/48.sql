# --- !Ups
CREATE TABLE CacheCrawler (
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  url VARCHAR(512) NOT NULL,
  dateInserted DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
  dateUpdated DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  title VARCHAR(256) NOT NULL,
  image VARCHAR(512) NOT NULL,
  description VARCHAR(512) NOT NULL,
  status ENUM('Queued', 'Done') NOT NULL DEFAULT 'Queued',
  UNIQUE KEY CacheCrawler_url_unique (url),
  INDEX CacheCrawler_dateUpdated_index (dateUpdated),
  INDEX CacheCrawler_status_index (status)
);

# --- !Downs
DROP TABLE CacheCrawler;
