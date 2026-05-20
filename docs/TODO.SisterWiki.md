# Sister Wiki TODO

## 목표

- [ ] 생성된 `See Also` 섹션에 sister wiki 추천을 추가한다.
- [ ] 첫 버전에서는 sister wiki 전용 위키 링크 문법을 추가하지 않는다.
- [ ] 다른 AhaWiki 사이트의 같은 이름 문서와 유사 문서를 자동으로 보여준다.
- [ ] private page는 기존 `SimilarPages`처럼 계산은 하되 렌더링에서 제외한다.
- [ ] sister wiki 유사 문서는 새 매크로를 만들지 않고 기존 `MacroSimilarPages`를 확장해서 보여준다.
- [ ] 같은 이름 문서는 별도 `MacroSisterPages`로 만들고 `See Also`의 다른 블록으로 보여준다.

## 표시 위치

- [ ] 같은 이름 문서는 `See Also` 자동 생성 마크업에 별도 `Sister Pages` 블록으로 추가한다.
- [ ] 유사 문서는 기존 `Similar Pages` 섹션과 `[[SimilarPages]]` 매크로를 확장한다.

```wiki
=== Sister Pages === #Sister-Pages-Generated.generated
[[SisterPages]]

=== Similar Pages === #Similar-Pages-Generated.generated
[[SimilarPages]]
```

- [ ] `Similar Pages` 내부에는 하위 heading을 추가하지 않는다.
- [ ] 동일 site의 비슷한 문서를 먼저 보여준다.
- [ ] 그 아래에 다른 site의 비슷한 문서를 자연스럽게 이어서 보여준다.

## 추천 종류

### 같은 이름 문서

- [ ] 현재 페이지와 이름이 같은 문서를 다른 AhaWiki site에서 찾는다.
- [ ] 기존 site별 `PageLatestSummary` 메모리 캐시를 사용한다.
- [ ] 현재 site는 결과에서 제외한다.
- [ ] 렌더링 시점에 anonymous user가 읽을 수 있는 페이지만 표시한다.
- [ ] 같은 이름 문서가 유사 문서 목록에도 나오면 중복 제거하지 않고 그대로 둔다.
- [ ] 링크 생성에는 대상 site의 대표 도메인을 사용한다.
- [ ] 같은 이름 문서는 `MacroSimilarPages`가 아니라 `MacroSisterPages`에서 담당한다.

참고 캐시:

- [ ] `AhaWikiCache.PageMeta.SeqPageLatestSummary`

### 유사 문서

- [ ] 다른 AhaWiki site에서 cosine similarity가 높은 문서를 찾는다.
- [ ] cross-site cosine similarity는 페이지 렌더링 시점에 계산하지 않는다.
- [ ] cross-site cosine similarity는 미리 계산해서 DB에 저장한다.
- [ ] score는 현재 `SimilarPages`처럼 사용자에게 보여준다.
- [ ] 렌더링 시점에 anonymous user가 읽을 수 있는 페이지만 표시한다.

## 데이터 모델

- [ ] 별도 `CalculatedSisterCosineSimilarity` 테이블을 만들지 않는다.
- [ ] 기존 `CalculatedCosineSimilarity`를 site-aware pair 구조로 확장한다.
- [ ] `site1`, `name1`, `site2`, `name2`, `similarity` 구조로 변경한다.
- [ ] `site1 == site2`이면 기존과 같은 내 site 내부 유사도로 취급한다.
- [ ] `site1 != site2`이면 sister wiki 유사도로 취급한다.
- [ ] 조회를 단순하게 하기 위해 양방향 row를 저장한다.
- [ ] 예: `A/Foo -> B/Bar`
- [ ] 예: `B/Bar -> A/Foo`

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

- [ ] `CalculatedCosineSimilarity`의 FK는 `Page`가 아니라 `PageMeta(site, name)`에 건다.
- [ ] `PageMeta`는 최신 페이지 단위 row이므로 similarity 대상 FK로 더 자연스럽다.
- [ ] `PageMeta`에 `canReadAnonymous` 같은 denormalized 필드를 추가할지 검토한다.
- [ ] 단, 초기 구현은 기존 권한 로직과 동일하게 렌더링 시점 필터링을 우선한다.

`canReadAnonymous`를 추가할 경우:

- [ ] `PageMeta` 계산 시 anonymous read 가능 여부를 함께 저장한다.
- [ ] 권한 변경 시 관련 `PageMeta.canReadAnonymous`를 갱신하거나 재계산한다.
- [ ] 렌더링 필터를 빠르게 만들 수 있는지 검증한다.
- [ ] 권한 규칙 변경과 캐시 무효화 비용이 과한지 확인한다.

## 대표 도메인

- [ ] `SiteDomain`에 여러 domain이 있으면 `created`가 가장 작은 row를 대표 도메인으로 사용한다.
- [ ] 보통 domain은 1개라는 전제를 둔다.
- [ ] 대표 도메인은 sister wiki 링크 생성에 사용한다.

## 공개 범위 규칙

- [ ] private page도 기존 `SimilarPages`처럼 계산 row는 만들 수 있다.
- [ ] 표시할 때만 target page가 anonymous readable인지 확인해서 제외한다.
- [ ] 같은 이름 문서도 렌더링 시점에 anonymous readable 여부로 필터링한다.
- [ ] private page 제목이 최종 HTML에 노출되지 않게 한다.

## 매크로

- [ ] `MacroSisterPages`를 추가한다.
- [ ] `MacroSisterPages`는 같은 이름 문서만 담당한다.
- [ ] `MacroSisterPages`는 후보가 없으면 아무것도 출력하지 않는다.
- [ ] `MacroSisterPages`는 매칭되는 site를 모두 보여준다.
- [ ] 기존 `MacroSimilarPages`를 확장해서 cross-site 유사 문서를 추가한다.
- [ ] `MacroSimilarPages`는 same-site 유사 문서를 기존처럼 먼저 렌더링한다.
- [ ] `MacroSimilarPages`는 cross-site 유사 문서를 그 아래에 이어서 렌더링한다.
- [ ] `MacroSimilarPages` 내부에는 `Same Site`, `Sister Wikis` 같은 하위 heading을 추가하지 않는다.
- [ ] cross-site 유사 문서는 상위 20개 정도로 제한한다.
- [ ] site 이름을 표시한다.
- [ ] page 제목을 표시한다.
- [ ] similarity score를 표시한다.

출력 예시:

```wiki
 1. [[PercentLinkTitle(0.91, SomePage, "")]] [[Trivial(term(3:2))]]
 * [https://example.org/w/Bar ExampleWiki: Bar] 0.82
```

## See Also 연동

- [ ] `Wiki.getAhaMarkAdditionalInfo`의 generated `See Also` 마크업에 `Sister Pages` 블록을 추가한다.
- [ ] `Sister Pages` 블록은 같은 이름 문서가 있을 때만 출력한다.
- [ ] 기존 `Similar Pages` 섹션은 유지한다.
- [ ] `has similar pages` 판단이 same-site 유사 문서뿐 아니라 sister wiki 유사 문서도 고려하도록 확장한다.
- [ ] sister wiki 결과만 있는 경우에도 `Similar Pages` 섹션이 출력되게 한다.
- [ ] sister wiki 결과가 없으면 기존 출력과 동일하게 동작한다.

현재 generated subsection:

- [ ] Schema
- [ ] Backlinks
- [ ] Similar Pages
- [ ] Adjacent Pages

제안 순서:

- [ ] Schema
- [ ] Backlinks
- [ ] Sister Pages
- [ ] Similar Pages
- [ ] Adjacent Pages

## 계산

- [ ] 기존 page calculation 흐름에 cross-site similarity 재계산을 연결한다.
- [ ] 페이지가 변경되면 `(currentSite, currentPage)`가 `site1/name1` 또는 `site2/name2`에 포함된 기존 row를 삭제한다.
- [ ] 현재 페이지의 term vector를 다른 site의 page term vector와 비교한다.
- [ ] `site1 == site2`인 내부 similarity도 기존 동작과 동일하게 유지한다.
- [ ] `site1 != site2`인 sister wiki similarity도 같은 테이블에 저장한다.
- [ ] threshold 이상인 row를 저장한다.
- [ ] reverse row도 함께 저장한다.
- [ ] `CalculatedTermFrequency`를 term vector source로 사용한다.
- [ ] 초기 threshold는 기존 내부 cosine과 같은 `similarity > 0.3`으로 시작한다.
- [ ] 실제 결과 품질을 보고 threshold를 조정한다.

## 캐싱

- [ ] 같은 이름 문서는 기존 `PageLatestSummary` 메모리 캐시를 사용한다.
- [ ] 유사 문서는 precomputed `CalculatedCosineSimilarity` row를 조회한다.
- [ ] 필요해지면 `MacroSimilarPages` 결과에 작은 메모리 캐시를 추가한다.

## 미결정 사항

- [ ] `PageMeta.canReadAnonymous`를 이번 작업에 포함할지, 후속 최적화로 둘지 결정한다.
- [ ] `canReadAnonymous`를 넣는다면 권한 변경 시 재계산 범위를 어떻게 잡을지 결정한다.
- [ ] sister wiki 유사 문서 개수를 5개로 고정할지 설정화할지 결정한다.

## 구현 체크리스트

- [ ] `CalculatedCosineSimilarity` schema migration 추가
- [ ] `CalculatedCosineSimilarity` model을 `site1/name1/site2/name2` 구조로 변경
- [ ] `CalculatedCosineSimilarity` FK를 `PageMeta(site, name)`로 연결
- [ ] 기존 site 내부 similarity 조회가 깨지지 않게 `site1 == currentSite AND site2 == currentSite` 조건 반영
- [ ] sister wiki similarity 조회 helper 추가
- [ ] 대표 도메인 조회 helper 추가
- [ ] cross-site cosine 재계산 로직 추가
- [ ] page save/delete/rename 계산 흐름에 재계산 연결
- [ ] `MacroSisterPages` 추가
- [ ] `ExtractConvertInjectMacro`에 `MacroSisterPages` 등록
- [ ] `MacroSimilarPages` 확장
- [ ] generated `See Also`에 `Sister Pages` 마크업 추가
- [ ] sister wiki 결과만 있어도 generated `Similar Pages` 섹션이 출력되게 조건 확장
- [ ] 같은 이름 매칭 unit test 추가
- [ ] 렌더링 시점 public/private filtering unit test 추가
- [ ] 내부 similarity 기존 동작 유지 test 추가
- [ ] cross-site cosine row insert/lookup unit test 추가
