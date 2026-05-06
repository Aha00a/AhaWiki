# 위키 텍스트 기반 Kanban 운영 문서 (현행)

Kanban은 **위키 텍스트를 원본(source of truth)** 으로 사용합니다. 화면에서는 보드 UI로 편집하고, 저장 시 다시 위키 텍스트로 직렬화합니다.

---

## 1) 카드 문법 (현행)

```wiki
=== Info
==== 작업 제목 ==== #cardid123
===== Property
 * DueDate: [2027-01-01]
 * Assignee
  * [User:Aha00a]
 * Attachment
  * [[Attachment(path/to/file.png)]]
===== Activity
 * [User:Aha00a] [2026-05-04]T10:36:17
  * Moved from '''Done''' to '''Info'''
```

- `=== 리스트명` : 컬럼
- `==== 카드 제목 ==== #cardId` : 카드(안정 ID 필수)
- `===== Property` : 속성 블록
- `===== Activity` : 활동 로그 블록

제약:
- 카드 ID(`#...`)는 생성 후 유지
- `Assignee` 키 표기 고정
- 날짜 포맷 `YYYY-MM-DD`

---

## 2) 저장/동기화 동작

- 저장 API: `POST /w/:pageName`
- 최신 리비전 조회: `GET /api/pageRevision/:pageName`
- 코멘트 렌더링: `POST /api/renderAhaMark/:pageName`

저장 시:
- Kanban 블록 외부 텍스트는 변경하지 않음
- 카드/리스트 순서를 그대로 직렬화
- `409` 충돌 발생 시 최신 리비전 재조회 후 **1회 자동 재시도**

카드 모달:
- 열기 시 URL hash를 `#<cardId>`로 동기화
- 닫기 시 해당 hash 제거

---

## 3) 리비전 코멘트 규칙

공통 접두어: `Kanban - <EventPrefix> - ...`

지원 액션:
- `list:add`, `list:rename`, `list:move`, `list:delete`
- `card:add`, `card:rename`, `card:move`, `card:delete`
- `card:comment:add`, `card:property:update`

세부 규칙:
- `card:comment:add` 코멘트 값은 첫 줄 기준 최대 80자(초과 시 `...`)
- `card:property:update` 값 직렬화
  - 단일값: 문자열
  - 다중값: `, ` join
  - 빈 값: `(empty)`
- `card:delete`는 카드가 삭제되므로 Activity에는 남기지 않음

---

## 4) Property 허용 키

- `Creator` (1)
- `DueDate` (1)
- `Assignee` (n)
- `Attachment` (n)

---

## 5) ToDo 이력

- [x] 라운드트립 회귀 테스트 강화
  - 내용
    - 텍스트 -> 모델 -> 텍스트 왕복 시 의미 보존 검증
    - 카드 ID 유지, 순서 유지, Kanban 블록 외부 미변경 검증
  - 산출물
    - fixture 5~8개
    - golden text 기대값

- [x] 409 충돌 자동 재시도 통합 테스트
  - 충돌시 변경사항 rollback후 toast -> refresh
  - 사용자가 재작업하도록 안내.

- [x] Activity/리비전 코멘트 경계 테스트
  - 내용
    - `card:comment:add` 80자 절단 확인
    - `card:property:update` 단일/다중/빈 값 직렬화 검증
    - 줄바꿈/이모지/한글 포함 케이스 검증
  - 산출물
    - 단위 테스트 8~12개

- [x] 카드 모달 hash 네비게이션 보강
  - 내용
    - `#<cardId>` 진입/닫기 기본 동작 유지
    - back/forward 네비게이션 일관성 보강
  - 산출물
    - 프런트 테스트 및 수동 체크리스트

- [x] 리비전 코멘트 가독성 개선
  - 긴 변경 내용 요약 템플릿 정리
  - 예상 공수: 0.5일

---

## 6) 카드 모달 Hash 네비게이션 수동 체크리스트

검증 일자: 2026-05-06

### 기본 동작
- [ ] 카드 클릭 시 URL hash가 `#<cardId>` 로 변경된다.
- [ ] 동일 카드가 열린 상태에서 재클릭해도 history stack이 불필요하게 늘어나지 않는다.
- [ ] 모달 닫기(X 버튼/배경 클릭) 시 hash가 제거되어 기본 페이지 URL로 복귀한다.

### 뒤로/앞으로 네비게이션
- [ ] 카드 열기 후 브라우저 뒤로 가기 시 모달이 닫힌다.
- [ ] 모달 닫기 후 브라우저 앞으로 가기 시 같은 카드 모달이 다시 열린다.
- [ ] 카드 A -> 카드 B 순서로 열었을 때 뒤로 가기 시 카드 A 상태로 복귀한다.

### 직접 진입
- [ ] 페이지를 `.../w/<page>#<cardId>`로 직접 열면 해당 카드 모달이 자동으로 열린다.
- [ ] 존재하지 않는 `#<cardId>`로 열면 오류 없이 일반 보드 화면이 유지된다.

### 데이터 변경 시나리오
- [ ] 모달에서 카드 제목 변경 후 닫아도 hash 동작이 정상 유지된다.
- [ ] 모달에서 카드 삭제 시 hash가 제거되고 stale hash로 남지 않는다.

### 회귀 점검 참고 자동 테스트
- `test/kanban.hash-navigation.test.mjs`
