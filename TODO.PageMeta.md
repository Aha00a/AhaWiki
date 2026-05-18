# TODO: PageMeta

## 1) DB 스키마 도입
- [ ] `PageMeta` 테이블 추가 - evolution 
  - site, name: PK
  - dateInserted - NOW()
  - dateUpdated - NOW()
  - datePageLastChanged
  - image - string? - 대표이미지
  - permRead - Page의 max(revision)의 permRead
  - size
- [ ] Calculate시 함께 생성
  - 본문 파싱후 다음 중 첫번째
    - #!Schema의 image
    - #!Schema의 logo
    - Attachment
    - MacroImage 
  - 없을때 Null, 표시할때 기본이미지
  - Recalculate는 PageMeta에 행이 없는 Row

## 3) 헤더 메타(SEO/SNS) 정식 연동
- [ ] `_baseSkeleton`의 `og:image`, `twitter:image` 등 추가
- [ ] 캐시 무효화 정책 확인 (페이지 수정 후 메타 반영 시점)

## 4) Adjacent Pages(그래프) 이미지 확장
- [ ] adjacentPages API 응답에 대표 이미지 필드 포함
- [ ] 노드 렌더러(D3/canvas)에서 썸네일 표시 옵션 추가
- [ ] 성능/가독성 검토
  - lazy loading

## 6) 운영/관리 기능
- [ ] Admin Page 목록
  - 서버측 페이징, 검색, 정렬
  - 페이지별 재계산 버튼

## 7) Page테이블의 몇몇 필드 삭제

