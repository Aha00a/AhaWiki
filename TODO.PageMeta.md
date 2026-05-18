# TODO: PageMeta


## 1) DB 스키마 도입
- [x] `PageMeta` 테이블 추가 - evolution
  - PK: `(site, name)`
  - `dateInserted` - NOW()
  - `dateUpdated` - PageMeta 계산 완료 시점
  - `datePageLastChanged` - Page의 최신 revision 시점
  - `image` - string? - 대표 이미지
    - 후보 우선순위: `#!Schema(image/logo)` → `Attachment` → `[[Image(...)]]` → `[[Attachment(...)]]`
    - 동순위 선택 규칙: 본문 출현 순서 기준 첫 번째
    - 이미지 미존재 시 `NULL` 저장, 렌더링에서 기본 이미지 사용
  - `permRead` - Page의 max(revision)의 permRead 스냅샷
  - `size` - Page의 max(revision)의 content length
- [x] 인덱스/조회 패턴 확정
  - Admin 목록용 정렬/검색 인덱스
  - `image is NULL`/`dateUpdated` 조건 배치 인덱스

## 2) Calculate/파서 연동
- [x] Calculate 파이프라인에서 `PageMeta` upsert 연동
- [x] 본문 파싱 후 대표 이미지 후보 추출기 구현
  - [x] Macro 문법 기준(`[[Image(...)]]`, `[[Attachment(...)]]`) 파싱 반영
- [x] 재계산 정책 
  - [x] 일반 Calculate: 대상 페이지의 `PageMeta` upsert
  - [x] Recalculate(관리자): 강제 재계산 모드 지원
  - [x] 배치 Recalculate: `PageMeta` 누락 row 조건 실행 옵션
  - [x] 주기적으로 스케쥴러로 Page에는 있지만 PageMeta에 없는 페이지 Calculate 호출.
  - [x] 페이지 생성, 수정, 업데이트, rename시 Calculate 호출
  - [x] 페이지 삭제시 페이지 메타 삭제

## 3) 헤더 메타(SEO/SNS) 정식 연동
- [x] `_baseSkeleton`의 `og:image`, `twitter:image` 등 추가
- [x] 메타 URL canonical/fallback 규칙 정리

## 4) Adjacent Pages(그래프) 이미지 확장
- [x] `adjacentPages` API 응답에 대표 이미지 필드 포함
  - [x] `imageUrl`
  - [x] `hasFallbackImage` (기본 이미지 여부)
- [x] 노드 렌더러(D3/canvas)에서 썸네일 표시 옵션 추가
- [x] 성능/가독성 검토
  - [x] lazy loading
  - [x] 초기 preload 개수 제한(예: 상위 N개)
  
## 5) ContextSite, AhaWikiCache, PageLogic 개선
- [ ] setPageName, seqPageByPermission 등의 필드를 Page테이블이 아닌 PageMeta 테이블을 이용하도록 개선
  - [x] setPageName을 PageMeta 기반 캐시(`PageMeta.SeqPageName`)로 전환
  - [x] seqPageByPermission을 PageMeta 기반으로 1차 전환(기본 메타 필드 중심)

## 6) 운영/관리 기능
- [ ] Admin Page 목록
  - 서버측 페이징, 검색, 정렬
  - 대표 이미지/최종 계산시각 표시
- [ ] 페이지별 재계산 버튼
  - 단건 강제 재계산

## 7) Page 테이블의 몇몇 필드 삭제
- [ ] 삭제 대상 필드 목록 확정
- [ ] 단계적 제거
  - [ ] `deprecated` 표시
  - [ ] 읽기 경로 `PageMeta` 전환 완료
  - [ ] fallback 제거 후 drop migration
