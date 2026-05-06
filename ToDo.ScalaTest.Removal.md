# ScalaTest Removal To-Do

- [ ] 현재 ScalaTest 사용 현황 인벤토리 작성 (`test/**/*.scala` 중 `org.scalatest` import/상속 구조 정리).
- [ ] 마이그레이션 대상 분류표 작성 (로직 테스트 / 모델 테스트 / 컨트롤러 테스트 / 유틸 테스트).
- [ ] `Test.unit` 내 테스트 섹션 구조 표준안 정의 (도메인별 함수 분리, 네이밍 규칙, 실패 메시지 규칙).
- [ ] 공통 assertion 유틸 정리 (`TestUtil.assertEquals` 확장 필요 여부, 컬렉션/예외/assertAll 패턴 정리).
- [ ] ScalaTest DSL(`AnyFreeSpec`, `in`, `mustBe` 등) → 순수 assertion 변환 규칙 문서화.
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
