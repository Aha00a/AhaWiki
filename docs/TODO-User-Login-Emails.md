# TODO: User Login Emails

## Goal

한 사람이 여러 이메일로 로그인해도 하나의 `User.seq`로 처리한다.

대표 사례:

- `aha00a@gmail.com`
- `aha00a@aharise.com`

`User.email`을 제거하고, 로그인 가능한 이메일 목록을 `UserEmail`에만 둔다.

## Direction

`User`는 사람 자체를 나타낸다.

```sql
User(
  seq,
  created,
  updated,
  nickname,
  profileImageUrl,
  ...
)
```

`UserEmail`은 그 사람이 로그인할 수 있는 이메일 목록이다.

```sql
UserEmail(
  user,
  email,
  isPrimary,
  created
)
```

규칙:

- `User.seq`가 사용자 identity다.
- `UserEmail.email`이 로그인 이메일의 source of truth다.
- 한 이메일은 하나의 `User`에만 연결된다.
- 대표 이메일이 필요하면 `UserEmail.isPrimary = true`로 표현한다.
- 세션 identity는 `seq` 중심으로 둔다. 필요하면 `loginEmail`은 표시/감사용으로만 저장한다.

## Migration

새 evolution에서 `UserEmail`을 다시 만들고 기존 `User.email`을 옮긴 뒤 `User.email`을 제거한다.

```sql
CREATE TABLE UserEmail (
    user int NOT NULL,
    email varchar(255) NOT NULL,
    isPrimary boolean NOT NULL DEFAULT false,
    created datetime DEFAULT CURRENT_TIMESTAMP NOT NULL,
    PRIMARY KEY (user, email),
    CONSTRAINT UserEmail_email_uindex UNIQUE (email),
    CONSTRAINT UserEmail_User_seq_fk FOREIGN KEY (user) REFERENCES User (seq)
);

INSERT INTO UserEmail (user, email, isPrimary)
SELECT seq, email, true
FROM User;

DROP INDEX User_email_uindex ON User;
ALTER TABLE User DROP COLUMN email;
```

주의:

- 과거 `UserEmail`은 `16.sql`에서 생성됐지만 `29.sql`에서 삭제됐다.
- 실제 FK/인덱스 이름은 운영 DB에서 확인한 뒤 migration에 반영한다.

## Code Changes

필수 변경:

- `UserEmail` model 추가
  - email로 user 조회
  - user의 로그인 이메일 목록 조회
  - 로그인 이메일 추가/삭제
  - primary email 변경
- `User.selectOrInsert` 변경
  - `UserEmail.email`로 user를 찾는다.
  - 없으면 `User` 생성 후 `UserEmail`에 primary email을 추가한다.
- `SessionLogic` 변경
  - 세션에서 identity는 `seq`, `nickname` 중심으로 둔다.
  - `email`은 제거하거나 `loginEmail`로 의미를 낮춘다.
- `WikiPermission` 변경
  - 단일 actor email이 아니라 현재 사용자의 모든 로그인 이메일로 권한을 평가한다.
  - `Exact`: 로그인 이메일 중 하나와 일치하면 match
  - `Domain`: 로그인 이메일 중 하나가 domain으로 끝나면 match
  - `Login`: `User.seq`가 있으면 match
- admin/user list/API query에서 `User.email` 참조 제거
  - 필요한 이메일 표시는 `UserEmail.isPrimary` join으로 가져온다.

## User UI

계정 설정에 로그인 이메일 UI를 추가한다.

```text
계정 설정

닉네임: aha00a

로그인 이메일
- aha00a@gmail.com        대표
- aha00a@aharise.com

[Google 계정 연결]
```

초기 구현은 Google OAuth 기반 연결만 지원한다.

- 텍스트 입력만으로 이메일을 추가하지 않는다.
- 사용자는 `Google 계정 연결`로 다른 이메일 소유권을 증명한다.
- 연결 callback은 일반 로그인 callback과 분리한다.
- 연결 callback은 현재 로그인된 `User.seq`를 반드시 요구한다.

## Duplicate Merge Flow

사용자가 이미 다른 `User`에 연결된 이메일을 현재 계정에 연결하려고 하면 병합 확인 화면을 보여준다.

```text
이미 다른 계정에 연결된 이메일입니다.

aha00a@aharise.com 은 이미 AhaWiki 계정에 연결되어 있습니다.
이 계정을 현재 계정과 병합할까요?

병합될 수 있는 항목:
- 작성 기록
- 방문 기록
- 첨부 파일
- 프로필 이미지

[병합하기] [취소]
```

사용자가 명시적으로 확인한 뒤에만 서버가 병합한다.

병합 시 처리할 주요 참조:

- `Page.user`
- `AccessLog.user`
- `UserViewHistory.user`
- `Attachment.user`
- `UserNicknameHistory.user`
- `UserNicknameHistory.changedBy`
- 그 외 `User(seq)` FK

## Verification Checklist

- [ ] `User.email` 없이 컴파일된다.
- [ ] 기존 `User.email`이 `UserEmail` primary email로 migration된다.
- [ ] `aha00a@gmail.com`과 `aha00a@aharise.com`이 같은 `User.seq`로 로그인된다.
- [ ] 계정 설정에서 로그인 이메일 목록이 보인다.
- [ ] Google OAuth로 새 로그인 이메일을 직접 연결할 수 있다.
- [ ] 이미 다른 user에 연결된 이메일은 병합 확인 화면을 거친다.
- [ ] 병합 취소 시 데이터가 바뀌지 않는다.
- [ ] 병합 확인 후 주요 FK가 canonical `User.seq`로 이동한다.
- [ ] 기존 이메일 기반 permission이 로그인 이메일 전체 기준으로 동작한다.
- [ ] profile image 업데이트가 어느 이메일 로그인에서도 동작한다.

## Open Questions

- primary email 삭제를 허용할 것인가?
- 최소 하나의 로그인 이메일을 반드시 남길 것인가?
- Google People API에서 여러 verified email을 받으면 모두 자동 등록할 것인가?
- 병합 후 duplicate `User` row를 삭제할지, 비활성 보존할지 결정해야 한다.
