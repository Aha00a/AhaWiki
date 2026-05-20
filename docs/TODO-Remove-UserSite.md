# TODO: Remove UserSite

## Goal

`UserSite` 테이블을 제거한다.

이 작업을 먼저 끝낸 뒤 `User.email` 제거와 로그인 이메일 병합 작업을 진행한다.

## Why

현재 `UserSite`는 신뢰 가능한 membership source가 아니다.

확인된 상태:

- 앱 코드에 `UserSite` insert/upsert 경로가 없다.
- 조회는 admin 목록/통계에서만 사용된다.
- 운영 DB의 row가 `2025-05-13 13:02~13:06` 근처에 몰려 있다.
- 이후 로그인/활동으로 갱신되는 흐름이 없다.

따라서 현재 데이터는 실제 membership이라기보다 잠깐 쓰다 만 스냅샷 또는 실험 데이터로 보는 것이 맞다.

## Current Usage

제거 전 정리해야 하는 사용처:

- `app/models/tables/UserSite.scala`
- `app/controllers/Admin.scala`
  - `siteUsers` action
- `app/views/Admin/siteUsers.scala.html`
- `app/controllers/Api.scala`
  - `adminSites`의 `userCount`
  - `adminSiteUsers`
  - `adminUsers`의 `site_count`
  - admin dashboard의 `siteUserCreated`
- `app/assets/js/admin.jsx`
  - `/Admin/SiteUsers`
  - `/Admin/SiteUser`
  - 관련 navigation/view/table

## Direction

`UserSite`를 membership으로 되살리지 않는다.

대체 기준:

- site별 작성자는 `Page.user`에서 확인한다.
- site별 열람자는 `UserViewHistory.user`에서 확인한다.
- site별 요청자는 필요하면 `AccessLog.user`에서 확인한다.
- "site member" 개념이 나중에 필요하면 새 요구사항으로 다시 설계한다.

현재 작업에서는 site membership 개념을 제거하고, stale table을 참조하지 않도록 만든다.

## Code Changes

필수 변경:

- `UserSite.scala` 삭제
- `Admin.siteUsers` action 삭제 또는 안전한 redirect/404 처리
- `siteUsers.scala.html` 삭제
- `Api.adminSiteUsers` 삭제 또는 사용하지 않게 정리
- `Api.adminSites.userCount` 제거 또는 실제 활동 기반 count로 대체
- `Api.adminUsers.site_count` 제거 또는 실제 활동 기반 count로 대체
- admin dashboard의 `siteUserCreated` 제거
- admin frontend에서 `/Admin/SiteUsers`, `/Admin/SiteUser` 관련 UI 제거

## Migration

새 evolution에서 `UserSite`를 제거한다.

```sql
DROP TABLE UserSite;
```

rollback은 schema 복원만 제공한다. 기존 데이터는 신뢰 가능한 source가 아니므로 복원 대상에서 제외해도 된다.

```sql
CREATE TABLE UserSite (
    user int NOT NULL,
    site int NOT NULL,
    created datetime DEFAULT CURRENT_TIMESTAMP NOT NULL,
    PRIMARY KEY (user, site),
    CONSTRAINT UserSite_Site_seq_fk FOREIGN KEY (site) REFERENCES Site (seq),
    CONSTRAINT UserSite_User_seq_fk FOREIGN KEY (user) REFERENCES User (seq)
);
```

## Verification Checklist

- [ ] `UserSite` 런타임 코드 참조가 사라진다.
- [ ] `UserSite.scala`가 삭제된다.
- [ ] admin frontend에서 `/Admin/SiteUsers`, `/Admin/SiteUser` 경로가 제거된다.
- [ ] admin build가 통과한다.
- [ ] Scala compile/test가 통과한다.
- [ ] admin sites 화면이 `UserSite.userCount`에 의존하지 않는다.
- [ ] admin users 화면이 `UserSite.site_count`에 의존하지 않는다.
- [ ] `TODO-User-Login-Emails.md`의 계정 병합 절차에서 `UserSite`를 고려하지 않는다.

## Follow-Up

`UserSite` 제거 후 `TODO-User-Login-Emails.md` 작업을 진행한다.

나중에 진짜 site membership이 필요해지면, 기존 `UserSite`를 되살리지 말고 요구사항을 다시 정의한 뒤 새로 설계한다.
