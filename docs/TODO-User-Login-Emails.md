# TODO: User Login Emails

## 작업 체크리스트

- [x] 1. 목표와 identity 원칙을 확정한다.

  한 사람이 여러 이메일로 로그인해도 하나의 `User.seq`로 처리한다.

  대표 사례:

  - `aha00a@gmail.com`
  - `aha00a@aharise.com`

  확정할 원칙:

  - `User.seq`가 사용자 identity다.
  - `UserEmail.email`이 로그인 이메일의 source of truth다.
  - 한 이메일은 하나의 `User`에만 연결된다.
  - 대표 이메일이 필요하면 `UserEmail.isPrimary = true`로 표현한다.
  - 세션 identity는 `seq` 중심으로 둔다. 필요하면 `loginEmail`은 표시/감사용으로만 저장한다.
  - `User.email`은 제거하고, 로그인 가능한 이메일 목록은 `UserEmail`에만 둔다.

- [x] 2. 구현 전에 정책 미정 항목을 결정한다.

  먼저 결정해야 뒤쪽 migration, UI, 병합 로직이 흔들리지 않는다.

  - primary email 삭제를 허용할 것인가?
  - 최소 하나의 로그인 이메일을 반드시 남길 것인가?
  - Google People API에서 여러 verified email을 받으면 모두 자동 등록할 것인가?
  - 병합 후 duplicate `User` row를 삭제할지, 비활성 보존할지 결정해야 한다.

  이번 구현의 결정:

  - 초기 UI에서는 email 삭제를 제공하지 않는다.
  - 따라서 최소 하나의 로그인 이메일은 migration/login/연결 flow에서 유지한다.
  - Google OAuth profile email만 연결하고, People API의 여러 verified email 자동 등록은 하지 않는다.
  - 병합 후 duplicate `User` row는 주요 FK와 `UserEmail`을 canonical `User.seq`로 이동한 뒤 삭제한다.

- [x] 3. 현재 스키마와 dev DB의 실제 제약 조건을 확인한다.

  과거 `UserEmail`은 `16.sql`에서 생성됐지만 `29.sql`에서 삭제됐다. 새 evolution을 만들기 전에 운영 DB의 실제 FK/인덱스 이름을 확인하고 migration에 반영한다.

  확인할 항목:

  - `User.email` 컬럼 존재 여부
  - `User_email_uindex` 인덱스의 실제 이름
  - 삭제된 `UserEmail` 관련 잔여 테이블/인덱스/제약 조건 존재 여부
  - `User(seq)`를 참조하는 모든 FK 목록

  `wiki_aha00a_com_dev` 확인 결과:

  - `User.email` 컬럼은 존재하지 않는다.
  - `UserEmail`은 `user`, `email`, `isPrimary`, `created` 컬럼을 가진다.
  - `UserEmail`에는 `(user, email)` primary key와 `UserEmail_email_uindex` unique index가 있다.
  - 현재 `User(seq)` 참조 FK는 `AccessLog.user`, `Attachment.user`, `Page.user`, `UserEmail.user`, `UserNicknameHistory.user`, `UserNicknameHistory.changedBy`, `UserViewHistory.user`다.
  - `Habit`과 `UserSite`는 현재 dev DB의 `User(seq)` 참조 목록에 없다.
  - user 수 55, login email 수 57, primary email 수 55이며, email이 없는 user는 없다.

  운영 DB의 최종 확인은 배포 전 작업으로 남긴다.

- [x] 4. `UserEmail` 스키마 migration을 작성한다.

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

  최종 migration에는 운영 DB에서 확인한 실제 인덱스/제약 조건 이름을 사용한다.

  구현 파일:

  - `conf/evolutions/default/56.sql`

- [x] 5. `UserEmail` model을 추가한다.

  필요한 기능:

  - email로 user 조회
  - user의 로그인 이메일 목록 조회
  - 로그인 이메일 추가
  - 로그인 이메일 삭제
  - primary email 변경
  - email 중복 여부 확인

  예상 위치:

  - `app/models/tables/UserEmail.scala`

- [x] 6. `User` model에서 `email` 의존성을 제거한다.

  `User`는 사람 자체를 나타내고, 이메일 목록은 `UserEmail`에서만 다룬다.

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

  변경할 항목:

  - `case class User`에서 `email` 제거
  - `User.IdEmailNickname` 같은 세션/API용 타입 재검토
  - `User.select` 계열 query에서 `U.email` 제거
  - primary email 표시가 필요한 경우 `UserEmail.isPrimary` join 사용

- [x] 7. `User.selectOrInsert`를 `UserEmail.email` 기준으로 변경한다.

  로그인 시 `User.email`이 아니라 `UserEmail.email`로 기존 사용자를 찾는다.

  처리 순서:

  - `UserEmail.email`로 user를 찾는다.
  - 없으면 `User`를 생성한다.
  - 새 `User`에 대해 `UserEmail` row를 추가한다.
  - 첫 이메일은 `isPrimary = true`로 저장한다.
  - profile image 업데이트는 어떤 이메일로 로그인해도 같은 `User.seq`에 반영되게 한다.

- [x] 8. 세션 구조를 `User.seq` 중심으로 정리한다.

  `SessionLogic`에서 identity는 `seq`, `nickname` 중심으로 둔다. 기존 `email`은 제거하거나 `loginEmail`로 의미를 낮춘다.

  확인할 항목:

  - 세션 저장 값
  - `RequestWrapper.getUser`
  - 관리자 판정에서 `email == "aha00a@gmail.com"`처럼 단일 이메일에 기대는 조건
  - 로그인한 이메일을 표시/감사 목적으로 남길 필요가 있는지

- [x] 9. Google OAuth 일반 로그인과 계정 연결 flow를 분리한다.

  초기 구현은 Google OAuth 기반 연결만 지원한다. 텍스트 입력만으로 이메일을 추가하지 않는다.

  일반 로그인:

  - Google profile email을 받는다.
  - `UserEmail.email` 기준으로 기존 user를 찾거나 새 user를 만든다.
  - `User.seq`로 세션을 만든다.

  계정 연결:

  - 현재 로그인된 `User.seq`를 반드시 요구한다.
  - 별도 callback으로 처리한다.
  - Google profile email의 소유권이 확인된 경우에만 현재 user에 연결한다.
  - 이미 다른 user에 연결된 email이면 바로 연결하지 않고 병합 확인 flow로 보낸다.

- [x] 10. 권한 평가를 현재 사용자의 모든 로그인 이메일 기준으로 변경한다.

  `WikiPermission`은 단일 actor email이 아니라 현재 사용자의 모든 로그인 이메일로 권한을 평가한다.

  기대 동작:

  - `Exact`: 로그인 이메일 중 하나와 일치하면 match
  - `Domain`: 로그인 이메일 중 하나가 domain으로 끝나면 match
  - `Login`: `User.seq`가 있으면 match

  관련해서 `provider.getUser.map(_.email)`처럼 단일 이메일을 넘기는 호출부를 모두 점검한다.

- [x] 11. `User.email`을 직접 참조하는 controller/view/API/admin 코드를 제거한다.

  확인할 주요 위치:

  - `app/controllers/Api.scala`
  - `app/controllers/Admin.scala`
  - `app/controllers/Wiki.scala`
  - `app/controllers/ApiCrawler.scala`
  - `app/views/_base.scala.html`
  - `app/views/Wiki/action.scala.html`
  - `app/assets/js/admin.jsx`
  - `conf/evolutions/default/30.sql`

  필요한 이메일 표시는 `UserEmail.isPrimary` join으로 가져온다.

  과거 evolution인 `30.sql`은 해당 시점 스키마에서 실행되어야 하므로 유지한다.

- [x] 12. 계정 설정에 로그인 이메일 UI를 추가한다.

  목표 화면:

  ```text
  계정 설정

  닉네임: aha00a

  로그인 이메일
  - aha00a@gmail.com        대표
  - aha00a@aharise.com

  [Google 계정 연결]
  ```

  UI에서 필요한 동작:

  - 현재 user의 로그인 이메일 목록 표시
  - primary email 표시
  - Google 계정 연결 버튼 제공
  - 삭제 허용 정책을 확정했다면 삭제 동작 제공
  - primary 변경 허용 정책을 확정했다면 대표 변경 동작 제공

- [x] 13. 이미 다른 user에 연결된 이메일의 병합 확인 flow를 구현한다.

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

  사용자가 명시적으로 확인한 뒤에만 서버가 병합한다. 취소 시 데이터가 바뀌면 안 된다.

  병합 화면은 Google OAuth 연결 callback에서 소유권이 확인된 이메일만 세션에 pending merge로 저장한 뒤 접근할 수 있다.

- [x] 14. user 병합 시 모든 `User(seq)` 참조를 canonical user로 옮긴다.

  병합 시 처리할 주요 참조:

  - `Page.user`
  - `AccessLog.user`
  - `UserViewHistory.user`
  - `Attachment.user`
  - `UserNicknameHistory.user`
  - `UserNicknameHistory.changedBy`
  - 그 외 `User(seq)` FK

  병합 후 duplicate `User` row는 삭제한다.

- [x] 15. 테스트와 검증 항목을 추가한다.

  최소 검증:

  - `User.email` 없이 컴파일된다.
  - 기존 `User.email`이 `UserEmail` primary email로 migration된다.
  - `aha00a@gmail.com`과 `aha00a@aharise.com`이 같은 `User.seq`로 로그인된다.
  - 계정 설정에서 로그인 이메일 목록이 보인다.
  - Google OAuth로 새 로그인 이메일을 직접 연결할 수 있다.
  - 이미 다른 user에 연결된 이메일은 병합 확인 화면을 거친다.
  - 병합 취소 시 데이터가 바뀌지 않는다.
  - 병합 확인 후 주요 FK가 canonical `User.seq`로 이동한다.
  - 기존 이메일 기반 permission이 로그인 이메일 전체 기준으로 동작한다.
  - profile image 업데이트가 어느 이메일 로그인에서도 동작한다.

  자동 검증:

  - 임시 복사본에서 `sbt compile` 성공
  - 임시 복사본에서 `sbt test` 성공
  - `UserMergeSpec`에서 duplicate `User` 삭제, `UserEmail`/일반 FK 이동, `UserSite` 충돌 처리를 검증한다.

- [x] 16. 배포 전후 운영 확인 절차를 준비한다.

  아래 항목은 절차 준비 완료 상태다. 실제 확인은 배포 전 staging/운영 DB에서 수행한다.

  배포 전:

  - DB 백업
  - staging에서 migration rehearsal
  - `UserEmail.email` unique 제약 검증
  - 기존 user 수와 migration된 primary email 수 비교
  - 현재 DB의 `User(seq)` 참조 FK 목록 확인
  - Google OAuth Console의 승인된 redirect URI에 `/google/oauth/callback` 등록 여부 확인

  배포 후:

  - 대표 계정 두 이메일로 각각 로그인 확인
  - 관리자 권한 확인
  - 이메일 기반 page permission 확인
  - 계정 설정의 로그인 이메일 목록 확인
  - 병합 flow를 staging 또는 제한된 계정으로 검증
