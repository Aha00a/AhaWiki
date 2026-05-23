# SiteAdmin 구현 TODO

## DB

- [x] evolution 파일 추가 (`conf/evolutions/default/61.sql`)
  - [x] `SiteAdmin` 테이블 생성 (`site`, `user`, `dateInserted`)
  - [x] `(site, user)` 복합 PK
  - [x] `site` → `Site(seq)` FK
  - [x] `user` → `User(seq)` FK

## Model / DAO

- [x] `app/models/tables/SiteAdmin.scala` 생성
- [x] `SiteAdminDao` 구현
  - [x] `exists(siteSeq, userSeq): Boolean`
  - [x] `findBySite(siteSeq): Seq[SiteAdmin]`
  - [x] `insert(siteSeq, userSeq)`
  - [x] `delete(siteSeq, userSeq)`

## Logic

- [x] `app/logics/AdminLogic.scala` 수정
  - [x] `isAdmin` → super admin (seq == 1) 체크 유지
  - [x] `isSiteAdmin(siteSeq, request)` 추가 (`isAdmin` 포함)

## Controller / API

- [x] Super admin 전용 SiteAdmin 관리 API 추가
  - [x] `GET    /api/Admin/Site/:seq/Admins` — 목록 조회
  - [x] `POST   /api/Admin/Site/:seq/Admins` — 추가
  - [x] `DELETE /api/Admin/Site/:seq/Admins/:userSeq` — 삭제
- [x] 사이트 범위 API에 `isSiteAdmin` 적용 (전체 시스템 작업은 `isAdmin` 유지)
  - [x] `adminPermissions`
  - [x] `adminUpsertPermission`
  - [x] `adminDeletePermission`
  - [x] `adminPermissionDiagnose`
  - [x] `adminPageMetaList`
  - [x] `adminSitePageNames`
  - [x] `adminSiteCalculate`

## UI

- [x] Super admin 화면에서 SiteAdmin 관리 UI 추가 (`/Admin/Site/:seq/Admins`)
  - [x] 사이트별 관리자 목록 표시
  - [x] 관리자 추가 (user seq 입력)
  - [x] 관리자 삭제

## 테스트

- [ ] `SiteAdminDao` 단위 테스트
- [ ] `AdminLogic.isSiteAdmin` 단위 테스트
- [ ] API 통합 테스트
