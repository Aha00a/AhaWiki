# --- !Ups

# Evolution 68 created UserNicknameChangeRequest without a table collation, so it took the
# database default (utf8mb4_unicode_ci) where every other table is utf8mb4_bin. The convert
# moves status and rejectReason onto bin, and the second statement puts requestedNickname
# back to the case-insensitive collation it shares with User.nickname, which the convert
# would otherwise have overwritten. The table is empty, so both are instant.
#
# No semicolons in these comments. Play splits the script on every one of them before the
# database sees it, so a comment with a semicolon becomes two statements, and the second
# one is not a comment. The first attempt to apply this evolution failed exactly that way.
ALTER TABLE UserNicknameChangeRequest CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_bin;
ALTER TABLE UserNicknameChangeRequest MODIFY requestedNickname VARCHAR(255) COLLATE utf8mb4_general_ci NOT NULL;

# --- !Downs

ALTER TABLE UserNicknameChangeRequest CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
ALTER TABLE UserNicknameChangeRequest MODIFY requestedNickname VARCHAR(255) COLLATE utf8mb4_general_ci NOT NULL;
