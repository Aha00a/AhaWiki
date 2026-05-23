# SiteAdmin 구현 TODO

## DB

- [x] evolution 파일 추가 (`conf/evolutions/default/61.sql`)
  - [x] `SiteAdmin` 테이블 생성 (`site`, `user`, `dateInserted`)
  - [x] `(site, user)` 복합 PK
  - [x] `site` → `Site(seq)` FK
  - [x] `user` → `User(seq)` FK

## Model / DAO

- [ ] `app/models/tables/SiteAdmin.scala` 생성
- [ ] `SiteAdminDao` 구현
  - [ ] `exists(siteSeq, userSeq): Boolean`
  - [ ] `findBySite(siteSeq): Seq[SiteAdmin]`
  - [ ] `insert(siteSeq, userSeq)`
  - [ ] `delete(siteSeq, userSeq)`

## Logic

- [ ] `app/logics/AdminLogic.scala` 수정
  - [ ] `isAdmin` → super admin (seq == 1) 체크 유지
  - [ ] `isSiteAdmin(siteSeq, request)` 추가 (`isAdmin` 포함)

## Controller / API

- [ ] Super admin 전용 SiteAdmin 관리 API 추가
  - [ ] `GET  /admin/site/:siteSeq/admins` — 목록 조회
  - [ ] `POST /admin/site/:siteSeq/admins` — 추가
  - [ ] `DELETE /admin/site/:siteSeq/admins/:userSeq` — 삭제
- [ ] 기존 Site 관련 컨트롤러에서 `isAdmin` → `isSiteAdmin` 으로 교체

## UI

- [ ] Super admin 화면에서 SiteAdmin 관리 UI 추가
  - [ ] 사이트별 관리자 목록 표시
  - [ ] 관리자 추가 (유저 검색 또는 seq 입력)
  - [ ] 관리자 삭제

## 테스트

- [ ] `SiteAdminDao` 단위 테스트
- [ ] `AdminLogic.isSiteAdmin` 단위 테스트
- [ ] API 통합 테스트
