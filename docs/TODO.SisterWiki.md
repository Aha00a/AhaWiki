# Sister Wiki TODO

## 목표

- [x] 생성된 `See Also` 섹션에 sister wiki 추천을 추가한다.
- [x] 첫 버전에서는 sister wiki 전용 위키 링크 문법을 추가하지 않는다.
- [x] 다른 AhaWiki 사이트의 같은 이름 문서와 유사 문서를 자동으로 보여준다.
- [x] private page는 기존 `SimilarPages`처럼 계산은 하되 렌더링에서 제외한다.
- [x] 같은 이름 문서는 별도 `MacroTwinPages`로 만들고 `See Also`의 다른 블록으로 보여준다.
- [x] sister wiki 유사 문서는 새 매크로를 만들지 않고 기존 `MacroSimilarPages`를 확장해서 보여준다.
- [x] site 약어는 `Site.abbr` 필드를 사용한다.
- [x] cross-site 링크 표기는 `Abbr:PageName` 형식을 사용한다.
- [x] `Site.abbr`는 unique 해야 한다.
- [x] `Abbr:PageName`과 같은 이름의 페이지가 현재 site에 존재하면 현재 site 페이지를 우선한다.
- [x] 현재 site에 해당 이름의 페이지가 없을 때만 twin page 링크로 해석한다.
- [x] site 대표 도메인은 `Site.mainDomain` 필드로 역정규화한다.
- [x] evolution schema 작업을 가장 먼저 수행한다.

## 표시 위치

- [x] 같은 이름 문서는 `See Also` 자동 생성 마크업에 별도 `Twin Pages` 블록으로 추가한다.
- [x] 유사 문서는 기존 `Similar Pages` 섹션과 `[[SimilarPages]]` 매크로를 확장한다.

```wiki
=== Twin Pages === #Twin-Pages-Generated.generated
[[TwinPages]]

=== Similar Pages === #Similar-Pages-Generated.generated
[[SimilarPages]]
```

- [x] `Similar Pages` 내부에는 하위 heading을 추가하지 않는다.
- [x] 동일 site의 비슷한 문서를 먼저 보여준다.
- [x] 그 아래에 다른 site의 비슷한 문서를 자연스럽게 이어서 보여준다.

## 추천 종류

### 같은 이름 문서

- [x] 현재 페이지와 이름이 같은 문서를 다른 AhaWiki site에서 찾는다.
- [x] 기존 site별 `PageLatestSummary` 메모리 캐시를 사용한다.
- [x] 현재 site는 결과에서 제외한다.
- [x] 렌더링 시점에 anonymous user가 읽을 수 있는 페이지만 표시한다.
- [x] 같은 이름 문서가 유사 문서 목록에도 나오면 중복 제거하지 않고 그대로 둔다.
- [x] 링크 생성에는 대상 site의 `mainDomain`을 사용한다.
- [x] 같은 이름 문서는 `MacroSimilarPages`가 아니라 `MacroTwinPages`에서 담당한다.

참고 캐시:

- [x] `AhaWikiCache.PageMeta.SeqPageLatestSummary`

### 유사 문서

- [x] 다른 AhaWiki site에서 cosine similarity가 높은 문서를 찾는다.
- [x] cross-site cosine similarity는 페이지 렌더링 시점에 계산하지 않는다.
- [x] cross-site cosine similarity는 미리 계산해서 DB에 저장한다.
- [x] score는 현재 `SimilarPages`처럼 사용자에게 보여준다.
- [x] cross-site 결과도 same-site와 동일하게 `PercentLinkTitle`과 high scored term들을 보여준다.
- [x] 렌더링 시점에 anonymous user가 읽을 수 있는 페이지만 표시한다.

## 데이터 모델

- [x] `Site`에 `abbr` 필드를 추가한다.
- [x] migration 시 기존 row의 `abbr` 값은 `name`과 동일하게 채운다.
- [x] `Site.abbr`에는 unique 제약을 건다.
- [x] `Site`에 `mainDomain` 필드를 추가한다.
- [x] migration 시 기존 row의 `mainDomain` 값은 `SiteDomain`에서 `created`가 가장 작은 domain으로 채운다.
- [x] 새 site 생성/관리 UI에서도 `abbr`와 `mainDomain`을 다룰 수 있게 한다.
- [x] 별도 `CalculatedSisterCosineSimilarity` 테이블을 만들지 않는다.
- [x] 기존 `CalculatedCosineSimilarity`를 site-aware pair 구조로 확장한다.
- [x] `site1`, `name1`, `site2`, `name2`, `similarity` 구조로 변경한다.
- [x] `site1 == site2`이면 기존과 같은 내 site 내부 유사도로 취급한다.
- [x] `site1 != site2`이면 sister wiki 유사도로 취급한다.
- [x] 조회를 단순하게 하기 위해 양방향 row를 저장한다.
- [x] 예: `A/Foo -> B/Bar`
- [x] 예: `B/Bar -> A/Foo`

제안 테이블 형태:

```sql
CREATE TABLE CalculatedCosineSimilarity (
  site1 INT NOT NULL,
  name1 VARCHAR(255) NOT NULL,
  site2 INT NOT NULL,
  name2 VARCHAR(255) NOT NULL,
  similarity DOUBLE NOT NULL,
  PRIMARY KEY (site1, name1, site2, name2),
  FOREIGN KEY (site1, name1) REFERENCES PageMeta (site, name),
  FOREIGN KEY (site2, name2) REFERENCES PageMeta (site, name)
);
```

## PageMeta

- [x] `CalculatedCosineSimilarity`의 FK는 `Page`가 아니라 `PageMeta(site, name)`에 건다.
- [x] `PageMeta`는 최신 페이지 단위 row이므로 similarity 대상 FK로 더 자연스럽다.
- [x] 권한 필터링은 기존 방식과 동일하게 렌더링 시점에 수행한다.

## 대표 도메인

- [x] 대표 도메인은 `Site.mainDomain`에 저장한다.
- [x] `Site.mainDomain`은 link render path에서 바로 사용한다.
- [x] `SiteDomain`에 여러 domain이 있으면 `created`가 가장 작은 row를 초기 `mainDomain`으로 사용한다.
- [x] 대표 도메인은 twin page와 cross-site similar page 링크 생성에 사용한다.
- [x] `SiteDomain`은 domain 목록/host 매핑 용도로 유지한다.
- [x] `Site.mainDomain`은 대표 링크 생성을 위한 역정규화 필드로 취급한다.
- [x] `Site.mainDomain`은 `SiteDomain`과 자동 동기화하지 않는 독립 필드로 관리한다.
- [x] `Site.mainDomain` 변경은 관리자가 수동으로 책임지고 관리한다.

## 공개 범위 규칙

- [x] private page도 기존 `SimilarPages`처럼 계산 row는 만들 수 있다.
- [x] 표시할 때만 target page가 anonymous readable인지 확인해서 제외한다.
- [x] 같은 이름 문서도 렌더링 시점에 anonymous readable 여부로 필터링한다.
- [x] private page 제목이 최종 HTML에 노출되지 않게 한다.

## 매크로

- [x] `MacroTwinPages`를 추가한다.
- [x] `MacroTwinPages`는 같은 이름 문서만 담당한다.
- [x] `MacroTwinPages`는 후보가 없으면 아무것도 출력하지 않는다.
- [x] `MacroTwinPages`는 매칭되는 site를 모두 보여준다.
- [x] 기존 `MacroSimilarPages`를 확장해서 cross-site 유사 문서를 추가한다.
- [x] `MacroSimilarPages`는 same-site 유사 문서를 기존처럼 먼저 렌더링한다.
- [x] `MacroSimilarPages`는 cross-site 유사 문서를 그 아래에 이어서 렌더링한다.
- [x] `MacroSimilarPages` 내부에는 `Same Site`, `Sister Wikis` 같은 하위 heading을 추가하지 않는다.
- [x] cross-site 유사 문서도 same-site와 동일한 출력 포맷을 사용한다.
- [x] cross-site 유사 문서도 `PercentLinkTitle(similarity, page, alias)`를 사용한다.
- [x] cross-site 유사 문서도 `PageLogic.selectHighScoredTerm`에 준하는 term 표시를 제공한다.
- [x] cross-site high scored term 조회는 `PageLogic.selectHighScoredTerm`를 site-aware 시그니처로 확장해서 처리한다.
- [x] cross-site 유사 문서는 기존 same-site `SimilarPages`와 동일하게 상위 20개를 표시한다.
- [x] site 이름을 표시한다.
- [x] cross-site page 표기는 `Site.abbr`를 사용해서 `Abbr:PageName`으로 표시한다.
- [x] page 제목을 표시한다.
- [x] similarity score를 표시한다.

출력 예시:

```wiki
 1. [[PercentLinkTitle(0.91, SomePage, "")]] [[Trivial(term(3:2))]]
 1. [[PercentLinkTitle(0.82, Example:Bar, "")]] [[Trivial(term(2:1))]]
```

## See Also 연동

- [x] `Wiki.getAhaMarkAdditionalInfo`의 generated `See Also` 마크업에 `Twin Pages` 블록을 추가한다.
- [x] `Twin Pages` 블록은 같은 이름 문서가 있을 때만 출력한다.
- [x] 기존 `Similar Pages` 섹션은 유지한다.
- [x] `has similar pages` 판단이 same-site 유사 문서뿐 아니라 sister wiki 유사 문서도 고려하도록 확장한다.
- [x] sister wiki 결과만 있는 경우에도 `Similar Pages` 섹션이 출력되게 한다.
- [x] sister wiki 결과가 없으면 기존 출력과 동일하게 동작한다.

현재 generated subsection:

- [ ] Schema
- [ ] Backlinks
- [ ] Similar Pages
- [ ] Adjacent Pages

제안 순서:

- [x] Schema
- [x] Backlinks
- [x] Twin Pages
- [x] Similar Pages
- [x] Adjacent Pages

## 계산

- [x] 기존 page calculation 흐름에 cross-site similarity 재계산을 연결한다.
- [x] 페이지가 변경되면 `(currentSite, currentPage)`가 `site1/name1` 또는 `site2/name2`에 포함된 기존 row를 삭제한다.
- [x] 현재 페이지의 term vector를 다른 site의 page term vector와 비교한다.
- [x] `site1 == site2`인 내부 similarity도 기존 동작과 동일하게 유지한다.
- [x] `site1 != site2`인 sister wiki similarity도 같은 테이블에 저장한다.
- [x] threshold 이상인 row를 저장한다.
- [x] reverse row도 함께 저장한다.
- [x] `CalculatedTermFrequency`를 term vector source로 사용한다.
- [x] 초기 threshold는 기존 내부 cosine과 같은 `similarity > 0.3`으로 시작한다.
- [ ] 실제 결과 품질을 보고 threshold를 조정한다.

## 캐싱

- [x] 같은 이름 문서는 기존 `PageLatestSummary` 메모리 캐시를 사용한다.
- [x] 유사 문서는 precomputed `CalculatedCosineSimilarity` row를 조회한다.
- [x] site 링크 생성에는 `Site.mainDomain`을 사용해서 render path에서 `SiteDomain` 조회를 피한다.
- [ ] 필요해지면 `MacroSimilarPages` 결과에 작은 메모리 캐시를 추가한다.

## 구현 체크리스트

- [x] evolution schema 작업을 가장 먼저 수행
- [x] `Site.abbr` schema migration 추가
- [x] 기존 `Site.abbr` 값을 `Site.name`과 동일하게 backfill
- [x] `Site.abbr` unique index 추가
- [x] `Site.mainDomain` schema migration 추가
- [x] 기존 `Site.mainDomain` 값을 `SiteDomain.created`가 가장 작은 domain으로 backfill
- [x] `Site` model에 `abbr` 필드 추가
- [x] `Site` model에 `mainDomain` 필드 추가
- [x] Admin site API에 `abbr` 반영
- [x] Admin site API에 `mainDomain` 반영
- [x] Admin site UI에 `abbr` 반영
- [x] Admin site UI에 `mainDomain` 반영
- [x] `CalculatedCosineSimilarity` schema migration 추가
- [x] `CalculatedCosineSimilarity` model을 `site1/name1/site2/name2` 구조로 변경
- [x] `CalculatedCosineSimilarity` FK를 `PageMeta(site, name)`로 연결
- [x] 기존 site 내부 similarity 조회가 깨지지 않게 `site1 == currentSite AND site2 == currentSite` 조건 반영
- [x] sister wiki similarity 조회 helper 추가
- [x] cross-site 링크 생성 시 `Site.mainDomain` 사용
- [x] cross-site cosine 재계산 로직 추가
- [x] page save/delete/rename 계산 흐름에 재계산 연결
- [x] `MacroTwinPages` 추가
- [x] `ExtractConvertInjectMacro`에 `MacroTwinPages` 등록
- [x] `MacroSimilarPages` 확장
- [x] generated `See Also`에 `Twin Pages` 마크업 추가
- [x] sister wiki 결과만 있어도 generated `Similar Pages` 섹션이 출력되게 조건 확장
- [x] cross-site similar page도 `PercentLinkTitle`과 high scored term을 표시하도록 구현
- [x] `PageLogic.selectHighScoredTerm`를 cross-site 조회가 가능하도록 확장
- [x] cross-site similar page 표시 개수를 20개로 구현
- [x] cross-site 링크 표기를 `Abbr:PageName`으로 구현
- [ ] 같은 이름 매칭 unit test 추가
- [ ] 렌더링 시점 public/private filtering unit test 추가
- [ ] 내부 similarity 기존 동작 유지 test 추가
- [ ] cross-site cosine row insert/lookup unit test 추가
