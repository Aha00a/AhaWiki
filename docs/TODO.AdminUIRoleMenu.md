# Admin UI 권한별 메뉴 표시 TODO

## 목표
현재 `/Admin`에서 전체Admin과 SiteAdmin 모두 동일한 메뉴가 표시되는 문제를 해결.
별도 라우트 없이, 기존 `/Admin` 화면에서 로그인한 사용자의 권한에 따라 메뉴를 다르게 표시.

## 네비게이션 메뉴 구조 (새 설계)

```
[Sites]
  ▼ AhaWiki (#1)   ← 현재 사이트: 자동 펼침 + 강조 표시
      Meta목록
      Config
      Cache
      Permission
      AccessLog
      SiteAdmins   ← 전체Admin만 표시
  ▶ OtherSite (#2) ← 전체Admin만 표시 (접힌 상태)
  ▶ OtherSite (#3) ← 전체Admin만 표시 (접힌 상태)

[전체 관리]        ← 전체Admin만 표시
  Dashboard
  User
  RecentChange
  AccessLog
  Cache (Crawler)
  S3 Browser
```

- **현재 사이트** = `request.host`로 판별 (서버에서 `/api/me`에 `currentSiteSeq` 포함)
- SiteAdmin에게는 Sites 섹션에 담당 사이트만 표시 (전체Admin은 전체 표시)
- 사이트별 서브메뉴는 현재 사이트/다른 사이트 모두 동일한 구조 → 컴포넌트 재사용
- SiteAdmin이 담당하지 않는 사이트의 `/Admin`에 접근 시 → Forbidden 유지 (서버 레벨)

---

## Backend

- [ ] `GET /api/me` API 추가 (`conf/routes`)
- [ ] `Api.scala`에 `me` 액션 구현
  - 로그인한 경우: `{ "loggedIn": true, "seq": 42, "nickname": "...", "loginEmail": "...", "profileImageUrl": "...", "isAdmin": false, "siteAdminSeqs": [1, 3], "currentSiteSeq": 2 }` 반환
    - `currentSiteSeq`: `request.host`로 찾은 현재 사이트의 seq
  - 미로그인: `{ "loggedIn": false }` 반환
- [ ] `Admin.scala` 컨트롤러: SiteAdmin도 `/Admin` 접근 허용하도록 수정
  - 현재는 `isAdmin`(전체Admin)만 허용 → `isSiteAdmin(currentSiteSeq)`도 허용

## Frontend — navigation.jsx

- [ ] `fetchJson("/api/me")` 호출해 `me` 상태 저장
- [ ] **[Sites]** 섹션
  - `me.isAdmin`이면 전체 사이트 목록 표시, 아니면 `me.siteAdminSeqs` 해당 사이트만 표시
  - `me.currentSiteSeq`에 해당하는 사이트는 자동 펼침 + 강조 표시
  - 사이트별 서브메뉴 컴포넌트 분리 (`SiteNavItem`) 하여 재사용
  - `SiteAdmins` 서브메뉴는 `me.isAdmin`일 때만 표시
- [ ] **[전체 관리]** 섹션 (`me.isAdmin`일 때만 표시)
  - Dashboard, User, RecentChange, AccessLog, Cache, S3
- [ ] Me API 로딩 중 네비게이션 skeleton 표시 (UX)

## Frontend — admin.jsx (AdminContent)

- [ ] `me.isAdmin`이 false이고 `page === "dashboard"`면 현재 사이트 정보 카드 표시
- [ ] `me.isAdmin`이 false이고 전체Admin 전용 page 접근 시 접근 불가 메시지 표시
  (`all-users`, `s3-browser`, `crawler-cache`, `recent-changes`, `access-logs`, `sites`)

## 참고

- 실제 보안은 이미 API 레벨(`isAdmin` / `isSiteAdmin`)에서 처리됨
- 프론트 메뉴 분기는 UX 목적 (전체Admin 기능을 SiteAdmin에게 노출하지 않음)
