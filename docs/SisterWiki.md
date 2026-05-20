# Sister Wiki

- Sister Wiki 기능은 여러 AhaWiki site 사이의 같은 이름 문서와 유사 문서를 `See Also` 영역에 함께 노출한다.
- 별도의 sister wiki 전용 위키 링크 문법은 추가하지 않고, cross-site 문서 표기는 `Site.abbr` 기반의 `Abbr:PageName` 형식을 사용한다.
- 실제 cross-site 링크 URL은 대상 site의 `Site.mainDomain`을 사용해 `https://{mainDomain}/w/{encodedPageName}` 형식으로 생성한다.
- `SiteDomain`은 host-to-site 매핑과 도메인 목록 관리 용도로 유지하고, 대표 링크 생성에는 `Site.mainDomain`을 사용한다.

## Site Metadata

- `Site` 모델은 `seq`, `name`, `abbr`, `mainDomain` 필드를 가진다.
- `Site.abbr`은 cross-site 문서 표시용 약어이며 unique 제약을 가진다.
- `Site.mainDomain`은 대표 도메인으로, twin page와 cross-site similar page 링크 생성에 사용한다.
- 기존 site의 `abbr`은 migration 시 `Site.name`으로 채워진다.
- 기존 site의 `mainDomain`은 migration 시 `SiteDomain.created`가 가장 작은 domain으로 채워진다.
- Admin site API와 UI에서 `abbr`, `mainDomain`을 조회하고 수정할 수 있다.
- site metadata 수정 후 `AhaWikiCacheMemoryDomainSite` 캐시를 무효화한다.

## Twin Pages

- 같은 이름 문서는 `MacroTwinPages`가 담당한다.
- `[[TwinPages]]`는 현재 문서 이름을 기준으로 다른 site에 같은 이름의 문서가 있는지 찾는다.
- `[[TwinPages(SomePage)]]`처럼 인자를 주면 지정한 문서 이름을 기준으로 찾는다.
- 현재 site는 후보에서 제외한다.
- 대상 site에 같은 이름의 최신 문서가 있어야 한다.
- 대상 문서는 anonymous user가 읽을 수 있는 경우에만 표시한다.
- 후보가 없으면 아무 HTML도 출력하지 않는다.
- 출력은 `Site.abbr:PageName` 텍스트와 대상 site의 `mainDomain` 기반 URL을 가진 목록으로 렌더링한다.

## Similar Pages

- 기존 `MacroSimilarPages`를 확장해 same-site 유사 문서와 sister wiki 유사 문서를 함께 표시한다.
- same-site 결과는 기존처럼 먼저 표시한다.
- cross-site 결과는 same-site 결과 뒤에 이어서 표시한다.
- same-site와 cross-site 결과 모두 각각 최대 20개까지 표시한다.
- cross-site 결과는 대상 문서가 anonymous user에게 읽기 가능한 경우에만 표시한다.
- cross-site 문서 표시는 `Abbr:PageName` 형식을 사용한다.
- `MacroPercentLinkTitle`은 현재 site에 같은 이름의 페이지가 있으면 현재 site 링크를 우선 사용한다.
- 현재 site에 해당 링크가 없고 `Abbr:PageName`의 `Abbr`이 다른 site의 `Site.abbr`과 일치하면 해당 site의 `mainDomain`으로 링크를 생성한다.
- 유사도는 `PercentLinkTitle(similarity, page, "")`로 표시한다.
- 공통 고득점 단어는 `PageLogic.selectHighScoredTerm`으로 조회해 `Trivial(term(freq1:freq2))` 형식으로 함께 표시한다.
- 유사 문서 row가 없으면 기존처럼 현재 문서 계산을 actor에 요청하고 빈 문자열을 반환한다.

## See Also

- generated `See Also` 영역은 다음 순서로 구성된다.
- `Schema`
- `Backlinks`
- `Twin Pages`
- `Similar Pages`
- `Adjacent Pages`
- `Twin Pages`는 같은 이름의 공개 twin page가 있을 때만 생성된다.
- `Similar Pages`는 same-site 또는 cross-site 유사 문서가 하나라도 있을 때 생성된다.
- cross-site 유사 문서만 있는 경우에도 `Similar Pages` 섹션이 생성된다.
- 관련 결과가 모두 없으면 generated `See Also` 마크업을 출력하지 않는다.

## Similarity Calculation

- `CalculatedCosineSimilarity`는 site-aware 구조로 동작한다.
- 테이블 키는 `(site1, name1, site2, name2)`이다.
- `site1 == site2` row는 기존 same-site 유사도로 취급한다.
- `site1 != site2` row는 sister wiki 유사도로 취급한다.
- FK는 최신 문서 단위인 `PageMeta(site, name)`을 참조한다.
- 계산 원천은 `CalculatedTermFrequency`의 term vector이다.
- similarity threshold는 `similarity > 0.3`이다.
- 자기 자신 row는 저장하지 않는다.
- 계산 시 현재 문서가 `site1/name1` 또는 `site2/name2`에 포함된 기존 row를 삭제한 뒤 다시 계산한다.
- 정방향 row와 역방향 row를 모두 저장해 조회를 단순화한다.
- `selectSameSite`는 현재 site 내부의 유사 문서만 조회한다.
- `selectCrossSite`는 현재 site에서 다른 site로 향하는 유사 문서만 조회한다.
- 기존 `select(name)`은 same-site 조회 동작을 유지한다.

## Permission

- private page도 계산 row의 대상이 될 수 있다.
- 렌더링 시점에는 대상 문서가 anonymous user에게 읽기 가능한지 확인한다.
- twin page와 cross-site similar page 모두 렌더링 전에 anonymous read 권한으로 필터링한다.
- 최종 HTML에는 읽을 수 없는 private page 제목이 노출되지 않도록 한다.

## Cache

- 같은 이름 문서 검색은 기존 `PageMeta.SeqPageLatestSummary` 메모리 캐시를 사용한다.
- 유사 문서 검색은 precomputed `CalculatedCosineSimilarity` row를 조회한다.
- cross-site 링크 생성은 `Site.mainDomain`을 사용해 렌더링 경로에서 `SiteDomain` 조회를 피한다.
- site/domain 매핑과 site 목록은 `AhaWikiCacheMemoryDomainSite`가 관리한다.

## Tests

- `MacroTwinPagesSpec`에서 다른 site의 같은 이름 문서 중 anonymous user가 읽을 수 있는 문서만 남기는 동작을 검증한다.
- `CalculatedCosineSimilaritySpec`에서 same-site row, cross-site row, reverse cross-site row 저장과 조회를 검증한다.
