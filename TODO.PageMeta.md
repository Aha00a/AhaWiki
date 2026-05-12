# TODO: PageMeta

## 1) DB 스키마 도입
- [ ] `PageMeta` 테이블 추가 - evolution 
  - site, name: PK
  - dateInserted - NOW()
  - dateUpdated - NOW() 
  - image - string? - 대표이미지
  - permRead - Page의 max(revision)의 permRead
- [ ] Calculated시 함께 생성
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
  - 작은 썸네일 프록시 혹은 리사이즈 URL

## 5) 링크 hover 미리보기 팝업
- [ ] crawler 내재화. 현재는 외부 서비스로 빠져있음. 외부링크에 대해 기 동작함.
- [ ] 링크 hover 시 페이지 요약 + 대표 이미지 카드 API 설계
- [ ] 기존 링크 처리 로직(`public/js/js.js`)에 프리뷰 훅 추가
- [ ] 접근성/모바일 대응
  - hover 없는 환경에서 long-press 또는 아이콘 트리거

## 6) 운영/관리 기능
- [ ] PageMeta 재계산 버튼(단일 페이지/전체 사이트)
