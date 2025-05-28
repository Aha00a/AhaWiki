# Default Schema

# --- !Ups

ALTER TABLE User
    ADD COLUMN nickname VARCHAR(255) COLLATE utf8mb4_general_ci;

alter table User
    modify updated datetime default CURRENT_TIMESTAMP not null on update CURRENT_TIMESTAMP;

UPDATE User
    SET nickname = LOWER(CONCAT(LEFT(SUBSTRING_INDEX(email, '@', 1), 3), '_', LPAD(FLOOR(100000 + RAND() * 899999), 6, '0')))
    WHERE nickname IS NULL;

ALTER TABLE User ADD CONSTRAINT User_nickname_lower_uindex UNIQUE (nickname);

ALTER TABLE User MODIFY COLUMN nickname VARCHAR(255) COLLATE utf8mb4_general_ci NOT NULL;


# --- !Downs

ALTER TABLE User
    DROP INDEX User_nickname_lower_uindex;

ALTER TABLE User
    DROP COLUMN nickname;
