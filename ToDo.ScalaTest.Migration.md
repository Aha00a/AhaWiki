# ScalaTest Migration To-Do
Wiki와 관련된 로직들을 Test.unit()으로 이관함을 목적으로 함.
추후 부분테스트 진행이 용이하도록 대상 Spec들을 각각의 파일로 분리하고, Test.unit()에서 호출한다.

- [x] 현재 ScalaTest 사용 현황 인벤토리 작성 (`test/**/*.scala` 중 `org.scalatest` import/상속 구조 정리).
- [x] 공통 assertion 유틸 정리 (`TestUtil.assertEquals` 확장 필요 여부, 컬렉션/예외/assertAll 패턴 정리).
- [x] 우선순위 1차 배치 선정 (변환 리스크 낮은 Spec부터).
- [x] 1차 배치 파일 마이그레이션 수행.
- [ ] 1차 배치에 대한 `/test/unit` 실행 경로 편입 및 회귀 확인.
- [ ] 2차 배치(의존성/fixture 복잡도 중간) 파일 마이그레이션 수행.
- [ ] 컨트롤러/Play 의존 테스트의 대체 전략 수립 (통합 테스트로 분리 or `Test.unit` 내 최소 검증으로 축소).
- [ ] 남은 고난도 Spec(비동기/환경의존) 처리 계획 확정.
- [ ] 전체 Spec 마이그레이션 완료 후 `test/**/*.scala` 내 ScalaTest import 잔여 여부 점검.
- [ ] `build.sbt`에서 ScalaTest 관련 의존성 제거.
- [ ] ScalaTest 제거 후 빌드/실행 영향 점검 및 문서 업데이트.
- [ ] 최종 검증 체크리스트 실행 (`/test/unit`, JS test, 핵심 기능 스모크).

## Notes
- 이번 문서는 **작업범위 정의용**이며, 실제 변환은 배치 단위로 진행.
- 각 배치 완료 시 체크박스를 업데이트.

### 대상 Spec
- [ ] test/models/tables/PermissionSpec.scala
- [ ] test/models/BlameSpec.scala
- [ ] test/models/JsonSpec.scala
- [ ] test/models/PageContentSpec.scala
- [ ] test/logics/SchemaOrgSpec.scala
- [x] test/logics/wikis/HeadingNumberSpec.scala
- [ ] test/logics/wikis/interpreters/InterpreterSchemaSpec.scala
- [ ] test/logics/wikis/interpreters/TraitInterpreterSpec.scala
- [x] test/logics/wikis/interpreters/InterpreterVimSpec.scala
- [ ] test/logics/wikis/interpreters/ahaMark/AhaMarkLinkSpec.scala
- [ ] test/logics/wikis/interpreters/InterpreterMarkdownSpec.scala
- [ ] test/logics/wikis/interpreters/InterpreterWikiSpec.scala
- [ ] test/logics/wikis/SignedReadUrlLogicSpec.scala
- [x] test/logics/wikis/macros/MacroColorCodeSpec.scala
- [x] test/logics/wikis/macros/MacroBrSpec.scala
- [x] test/logics/wikis/macros/MacroUptimeSpec.scala
- [x] test/logics/wikis/macros/TraitMacroSpec.scala
- [ ] test/logics/wikis/macros/MacroPeriodSpec.scala
- [x] test/logics/UrlDetectorSpec.scala
- [ ] test/logics/PermissionLogicSpec.scala



### assertion 유틸 정리 결과 (2026-05-06)
- `Test.unit` 경로(`app/controllers/Test.scala`)는 이미 `TestUtil.assertEquals` 중심으로 동작하며 문자열(CRLF 정규화)과 `Seq` 비교 오버로드를 보유함.
- ScalaTest Spec들에서 실제로 많이 쓰는 패턴은 대부분 기본 `assert(...)` 형태이며, 우선순위 대상 20개 파일 기준으로 `intercept`, matcher DSL(`should`, `must`, `mustBe`) 의존도는 매우 낮음.
- 따라서 1차 마이그레이션에서는 `TestUtil` 확장 없이도 진행 가능하며, 필요 시 아래 2가지만 보강하는 방식으로 최소 변경을 권장함.
  - `assertTrue(condition, message)`/`assertFalse(...)` 얇은 래퍼
  - `assertThrows[T](...)` (현재 배치 대상에서 사용되기 시작할 때만 도입)
- 결론: 지금 단계에서는 `TestUtil` 코드 변경 없이 배치 마이그레이션을 먼저 진행하고, 예외/집계 assertion은 실제 전환 중 등장 시점에 추가한다.

### 1차 배치 선정 (2026-05-06)
- 선정 기준
  - 순수 함수/정적 로직 중심(Play DI/DB fixture 의존 낮음)
  - `assert(...)` 위주로 변환 난이도가 낮은 파일 우선
  - 테스트 케이스 수가 작고 기대값이 명확한 Spec 우선
- 1차 배치 대상
  - test/logics/wikis/HeadingNumberSpec.scala
  - test/logics/wikis/interpreters/InterpreterVimSpec.scala
  - test/logics/wikis/macros/MacroBrSpec.scala
  - test/logics/wikis/macros/MacroColorCodeSpec.scala
  - test/logics/wikis/macros/MacroUptimeSpec.scala
  - test/logics/wikis/macros/TraitMacroSpec.scala
  - test/logics/UrlDetectorSpec.scala
- 2차 이후로 미루는 대상(사유)
  - `InterpreterWikiSpec`, `InterpreterSchemaSpec`, `PermissionLogicSpec`: 케이스 수/연관 로직이 커서 회귀 범위가 넓음
  - `PageContentSpec`, `BlameSpec`, `JsonSpec`, `PermissionSpec`: 도메인 fixture/파싱 케이스가 상대적으로 많음
