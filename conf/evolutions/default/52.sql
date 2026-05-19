# --- !Ups

ALTER TABLE Permission
    MODIFY COLUMN targetType ENUM('All', 'Exact', 'StartsWith', 'EndsWith', 'RegularExpression') NOT NULL DEFAULT 'Exact';


# --- !Downs

UPDATE Permission
SET targetType = 'Exact'
WHERE targetType = 'RegularExpression';

ALTER TABLE Permission
    MODIFY COLUMN targetType ENUM('All', 'Exact', 'StartsWith', 'EndsWith') NOT NULL DEFAULT 'Exact';
