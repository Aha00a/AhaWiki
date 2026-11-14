# --- !Ups

# Old accounts get the nickname a new account gets (owner's decision, 2026-11-15).
#
# When User.nickname was added (2025-05) every existing account was given the first three letters
# of its e-mail's local part, an underscore and six digits, such as abc_123456. Accounts made since
# 2026-05-11 get the whole local part (User.insert). This gives the old ones the same: the local
# part of the primary e-mail, lower case, anything but a-z 0-9 _ as _, runs of _ as one, no _ at
# either end -- the rule of User.insert for the addresses there are.
#
# Each rename is first written to UserNicknameHistory, as an approval would, by the super admin
# (AdminLogic.SuperAdminUserSeq), and the second statement renames exactly the accounts that got a
# history row here. A name another account already has, or that two of these accounts would share,
# or that comes out empty, is left out -- that account keeps its old name.
#
# Pages that name an old nickname (User:abc_123456) are not rewritten, as for any rename
# (UserNicknameChange). On 2026-11-15 the dev copy of production had none, and no pending nickname
# request from any of these accounts.

INSERT INTO UserNicknameHistory (`user`, `old`, `new`, changedBy)
SELECT u.seq, u.nickname, n.new, 1
  FROM User u
  JOIN (SELECT e.`user`,
               TRIM(BOTH '_' FROM REGEXP_REPLACE(REGEXP_REPLACE(LOWER(SUBSTRING_INDEX(e.email, '@', 1)), '[^a-z0-9_]', '_'), '_+', '_')) AS new
          FROM UserEmail e
         WHERE e.isPrimary = 1) n ON n.`user` = u.seq
 WHERE u.nickname REGEXP '^[a-z]{3}_[0-9]{6}$'
   AND n.new <> ''
   AND NOT EXISTS (SELECT 1 FROM User o WHERE o.nickname = n.new AND o.seq <> u.seq)
   AND (SELECT COUNT(*)
          FROM User u2
          JOIN UserEmail e2 ON e2.`user` = u2.seq AND e2.isPrimary = 1
         WHERE u2.nickname REGEXP '^[a-z]{3}_[0-9]{6}$'
           AND TRIM(BOTH '_' FROM REGEXP_REPLACE(REGEXP_REPLACE(LOWER(SUBSTRING_INDEX(e2.email, '@', 1)), '[^a-z0-9_]', '_'), '_+', '_')) = n.new) = 1;

UPDATE User u
  JOIN UserNicknameHistory h ON h.`user` = u.seq AND h.`old` = u.nickname AND h.changedBy = 1
   SET u.nickname = h.`new`, u.dateNicknameChanged = NOW()
 WHERE u.nickname REGEXP '^[a-z]{3}_[0-9]{6}$';

# --- !Downs

UPDATE User u
  JOIN UserNicknameHistory h ON h.`user` = u.seq AND h.`new` = u.nickname AND h.changedBy = 1
   SET u.nickname = h.`old`
 WHERE h.`old` REGEXP '^[a-z]{3}_[0-9]{6}$';

DELETE FROM UserNicknameHistory WHERE changedBy = 1 AND `old` REGEXP '^[a-z]{3}_[0-9]{6}$';
