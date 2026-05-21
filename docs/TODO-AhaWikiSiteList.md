# TODO: AhaWiki Site List Macro

## Decisions

- [x] `MacroAhaWikiSiteList`를 추가한다.
  - 위키 문법에서는 `[[AhaWikiSiteList]]`로 사용한다.
  - 출력은 제목 없이 목록만 렌더링한다.
  - `[[Backlinks]]`처럼 본문 안에서 필요한 위치에 붙여 쓸 수 있게 한다.

- [x] 사이트 목록은 `Site` 테이블 기반으로 관리한다.
  - 코드에 정적 사이트 목록을 박아두지 않는다.
  - 사이트 추가/삭제/정렬은 DB 값과 관리자 UI로 관리한다.

- [x] 공개 목록 노출 여부와 정렬값을 `Site.publicListedOrder` 하나로 표현한다.
  - 타입은 `DECIMAL(10,2) NULL`로 둔다.
  - `NULL`은 노출하지 않는다.
  - `0`은 노출하지 않는다.
  - `0`보다 큰 값만 공개 목록에 노출한다.

- [x] 정렬은 큰 값이 먼저 나오도록 한다.
  - 기본 정렬은 `publicListedOrder DESC, seq ASC`로 한다.
  - 같은 order 값에서는 오래된 사이트가 먼저 나오도록 `seq ASC`를 보조 정렬로 둔다.

- [x] 목록은 3단으로 보여준다.
  - 데스크톱에서는 3단 목록으로 출력한다.
  - 모바일에서는 1단으로 줄인다.

- [x] 사이트별 favicon을 함께 보여준다.
  - 우선 각 사이트의 favicon 설정을 사용할 수 있는지 검토한다.
  - 설정 favicon을 바로 쓰기 어렵다면 `https://{mainDomain}/favicon.ico`를 기본값으로 사용한다.
  - 이미지 로딩 실패 시 `/public/favicon.png`로 fallback한다.

## Data Model

- [x] `Site` 테이블에 컬럼을 추가한다.
  - `publicListedOrder DECIMAL(10,2) NULL`

- [x] evolution SQL을 추가한다.
  - `ALTER TABLE Site ADD COLUMN publicListedOrder DECIMAL(10,2) NULL;`
  - rollback SQL에서는 컬럼을 제거한다.

- [x] `models.tables.Site` case class에 필드를 추가한다.
  - `publicListedOrder: Option[BigDecimal]`
  - 기존 `select()` parser와 tuple mapping을 같이 수정한다.

- [x] `Site.updateAbbrAndMainDomain` 또는 별도 update API에 `publicListedOrder` 갱신을 포함할지 결정한다.
  - 기존 사이트 기본 정보 수정 API에 포함하면 관리자 UI가 단순해진다.
  - 별도 API로 두면 변경 범위가 작아진다.

## Macro

- [x] `MacroAhaWikiSiteList`는 DB에서 공개 사이트만 조회한다.
  - 조건: `publicListedOrder IS NOT NULL AND publicListedOrder > 0`
  - 정렬: `publicListedOrder DESC, seq ASC`

- [x] 매크로는 block macro로 등록한다.
  - `override def isBlock: Boolean = true`
  - `ExtractConvertInjectMacro`의 매크로 목록에 추가한다.

- [x] 출력 HTML은 목록만 포함한다.
  - wrapper: `<ul class="MacroAhaWikiSiteList">`
  - item: favicon 이미지 + 외부 링크
  - 링크 텍스트는 `mainDomain`을 기본으로 한다.

- [x] 링크와 이미지 URL은 HTML attribute escape를 적용한다.
  - `href`
  - `src`
  - fallback favicon 경로

- [x] `extractLink`에서 공개 사이트 URL 목록을 반환한다.
  - 계산 링크에 외부 사이트 URL이 잡히도록 한다.

## Favicon

- [ ] 사이트별 favicon 설정을 다른 사이트 기준으로 읽을 수 있는 헬퍼를 검토한다.
  - 현재 `AhaWikiConfig().site.favicon()`은 현재 context site 기준이다.
  - 공개 목록은 여러 사이트의 favicon이 필요하다.

- [x] 1차 구현은 `https://{mainDomain}/favicon.ico` fallback을 허용한다.
  - 사이트별 설정 favicon 지원은 후속 개선으로 분리할 수 있다.
  - 브라우저에서 직접 favicon을 가져오므로 서버 프록시나 S3 presigned URL 갱신 문제를 피할 수 있다.

- [x] 이미지 실패 fallback을 둔다.
  - `onerror="this.onerror=null;this.src='/public/favicon.png';"`

## CSS

- [x] `.MacroAhaWikiSiteList` 스타일을 추가한다.
  - `column-count: 3`
  - `column-gap: 16px`
  - `break-inside: avoid`

- [x] 모바일에서는 1단으로 보여준다.
  - `@media @widthTablet { column-count: 1; }`

- [x] favicon 크기를 고정한다.
  - `width: 16px`
  - `height: 16px`
  - `object-fit: contain`

## Admin UI

- [x] Admin Sites 화면에서 `publicListedOrder`를 관리할 수 있게 한다.
  - 빈 값은 `NULL`로 저장한다.
  - `0`은 저장 가능하지만 목록에서는 숨긴다.
  - 양수 값은 목록에 노출한다.

- [x] 입력 UI는 숫자 input으로 둔다.
  - step: `0.01`
  - min: `0`
  - placeholder: empty means hidden

- [x] 사이트 목록 테이블에 현재 공개 정렬값을 보여준다.
  - 공개 목록 노출 여부를 빠르게 확인할 수 있게 한다.

## Initial Data

- [x] 운영 DB에서 공개할 사이트에 order 값을 넣는다.
  - `ahawiki.net`
  - `aha00a.com`
  - `wiki.aharise.com`
  - `wiki.millpoo.com`
  - `fuerinha.ahawiki.net`
  - `oc.ahawiki.net`
  - `hwan.ahawiki.net`

- [x] 초기 order 정책을 정한다.
  - 예: `100.00`, `90.00`, `80.00` 순서
  - 중간 삽입을 위해 10 단위 간격을 둔다.

## Tests

- [x] `Site.publicListedOrder` parser 테스트를 추가한다.
  - `NULL`
  - `0`
  - 양수
  - 소수점 값

- [x] 공개 사이트 조회 로직 테스트를 추가한다.
  - `NULL` 제외
  - `0` 제외
  - 양수 포함
  - `publicListedOrder DESC, seq ASC` 정렬 확인

- [x] `MacroAhaWikiSiteList` 렌더링 테스트를 추가한다.
  - block macro 등록 확인
  - `<ul class="MacroAhaWikiSiteList">` 출력 확인
  - favicon 이미지 출력 확인
  - 외부 링크 속성 확인
  - HTML escaping 확인

- [ ] CSS 변경은 렌더링 결과를 브라우저에서 확인한다.
  - 데스크톱 3단
  - 모바일 1단
  - favicon 로딩 실패 fallback

## Deployment Checks

- [ ] migration 적용 전 운영 DB 백업을 확인한다.
- [ ] `Site.publicListedOrder` 컬럼이 추가되었는지 확인한다.
- [ ] 공개할 사이트에만 양수 order 값을 넣는다.
- [ ] 비공개/테스트 사이트는 `NULL` 또는 `0`인지 확인한다.
- [ ] `[[AhaWikiSiteList]]`가 운영 페이지에서 의도한 순서대로 출력되는지 확인한다.
- [ ] favicon이 깨지는 사이트가 있으면 `/favicon.ico` 존재 여부 또는 사이트별 favicon 설정을 확인한다.
