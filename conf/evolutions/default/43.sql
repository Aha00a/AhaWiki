# Default Schema

# --- !Ups

DROP TABLE IF EXISTS Habit;

# --- !Downs

CREATE TABLE Habit (
                       seq BIGINT NOT NULL AUTO_INCREMENT,
                       site INT NOT NULL,
                       user INT NOT NULL,
                       type ENUM('Sleep', 'WakeUp', 'Meal', 'Smoke') NOT NULL,
                       dateInserted DATETIME NOT NULL DEFAULT NOW(),
                       dateYmd DATE GENERATED ALWAYS AS (DATE(dateInserted)) STORED,
                       CONSTRAINT Habit_pk PRIMARY KEY (seq),
                       CONSTRAINT Habit_Site_seq_fk FOREIGN KEY (site) REFERENCES Site (seq) ON DELETE CASCADE,
                       CONSTRAINT Habit_User_seq_fk FOREIGN KEY (user) REFERENCES User (seq) ON DELETE CASCADE
);

CREATE INDEX Habit_site_type_dateInserted_index ON Habit (site, type, dateInserted);
CREATE INDEX Habit_site_user_dateInserted_index ON Habit (site, user, dateInserted);
CREATE INDEX Habit_user_type_dateInserted_index ON Habit (user, type, dateInserted);
CREATE INDEX Habit_site_dateYmd_type_index ON Habit (site, dateYmd, type);
CREATE INDEX Habit_site_user_dateYmd_index ON Habit (site, user, dateYmd);

