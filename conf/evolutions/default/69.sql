# --- !Ups

# Evolution 68 created UserNicknameChangeRequest without a table collation, so it took the
# database default — utf8mb4_unicode_ci — while every other table is utf8mb4_bin. The
# convert moves the columns without a collation of their own (status, rejectReason) onto
# bin; requestedNickname is then put back to the case-insensitive collation it shares with
# User.nickname, which the convert would otherwise have overwritten. The table is empty at
# this point, so both statements are instant.
ALTER TABLE UserNicknameChangeRequest CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_bin;
ALTER TABLE UserNicknameChangeRequest MODIFY requestedNickname VARCHAR(255) COLLATE utf8mb4_general_ci NOT NULL;

# --- !Downs

ALTER TABLE UserNicknameChangeRequest CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
ALTER TABLE UserNicknameChangeRequest MODIFY requestedNickname VARCHAR(255) COLLATE utf8mb4_general_ci NOT NULL;
