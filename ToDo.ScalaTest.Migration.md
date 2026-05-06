# ScalaTest Migration To-Do
Wiki와 관련된 로직들을 Test.unit()으로 이관함을 목적으로 함.
추후 부분테스트 진행이 용이하도록 대상 Spec들을 각각의 파일로 분리하고, Test.unit()에서 호출한다.

- [x] 현재 ScalaTest 사용 현황 인벤토리 작성 (`test/**/*.scala` 중 `org.scalatest` import/상속 구조 정리).
- [ ] 공통 assertion 유틸 정리 (`TestUtil.assertEquals` 확장 필요 여부, 컬렉션/예외/assertAll 패턴 정리).
- [ ] 우선순위 1차 배치 선정 (변환 리스크 낮은 Spec부터).
- [ ] 1차 배치 파일 마이그레이션 수행.
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
- [ ] test/logics/wikis/HeadingNumberSpec.scala
- [ ] test/logics/wikis/interpreters/InterpreterSchemaSpec.scala
- [ ] test/logics/wikis/interpreters/TraitInterpreterSpec.scala
- [ ] test/logics/wikis/interpreters/InterpreterVimSpec.scala
- [ ] test/logics/wikis/interpreters/ahaMark/AhaMarkLinkSpec.scala
- [ ] test/logics/wikis/interpreters/InterpreterMarkdownSpec.scala
- [ ] test/logics/wikis/interpreters/InterpreterWikiSpec.scala
- [ ] test/logics/wikis/SignedReadUrlLogicSpec.scala
- [ ] test/logics/wikis/macros/MacroColorCodeSpec.scala
- [ ] test/logics/wikis/macros/MacroBrSpec.scala
- [ ] test/logics/wikis/macros/MacroUptimeSpec.scala
- [ ] test/logics/wikis/macros/TraitMacroSpec.scala
- [ ] test/logics/wikis/macros/MacroPeriodSpec.scala
- [ ] test/logics/UrlDetectorSpec.scala
- [ ] test/logics/PermissionLogicSpec.scala


