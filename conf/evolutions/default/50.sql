# --- !Ups

ALTER TABLE Permission
    ADD COLUMN targetType ENUM('All', 'Exact', 'StartsWith', 'EndsWith') NOT NULL DEFAULT 'Exact' AFTER target,
    ADD COLUMN actorType ENUM('All', 'Login', 'Exact', 'Domain') NOT NULL DEFAULT 'Exact' AFTER actor,
    ADD COLUMN dateInserted DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP AFTER action,
    ADD COLUMN dateUpdated DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP AFTER dateInserted;

UPDATE Permission
SET dateInserted = created,
    dateUpdated = created;

UPDATE Permission
SET targetType =
        CASE
            WHEN target = '' THEN 'All'
            WHEN target LIKE '%?' THEN 'StartsWith'
            ELSE 'Exact'
        END,
    actorType =
        CASE
            WHEN actor = '' THEN 'All'
            WHEN actor = 'login' THEN 'Login'
            WHEN actor LIKE '@%' THEN 'Domain'
            ELSE 'Exact'
        END;

CREATE INDEX Permission_site_index ON Permission (site);

ALTER TABLE Permission
    DROP PRIMARY KEY;

UPDATE Permission
SET target = LEFT(target, CHAR_LENGTH(target) - 1)
WHERE targetType = 'StartsWith';

UPDATE Permission
SET actor = ''
WHERE actorType IN ('All', 'Login');

ALTER TABLE Permission
    DROP COLUMN created,
    ADD PRIMARY KEY (site, targetType, target, actorType, actor);


# --- !Downs

ALTER TABLE Permission
    ADD COLUMN created DATETIME DEFAULT NOW() NOT NULL AFTER action;

UPDATE Permission
SET created = dateInserted;

ALTER TABLE Permission
    DROP PRIMARY KEY;

UPDATE Permission
SET target = CONCAT(target, '?')
WHERE targetType = 'StartsWith';

ALTER TABLE Permission
    DROP COLUMN targetType,
    DROP COLUMN actorType,
    DROP COLUMN dateInserted,
    DROP COLUMN dateUpdated,
    ADD PRIMARY KEY (site, target, actor);

DROP INDEX Permission_site_index ON Permission;
