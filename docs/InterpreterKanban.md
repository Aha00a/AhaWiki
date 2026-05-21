# 위키 텍스트 기반 Kanban 운영 문서 (현행)

Kanban은 **위키 텍스트를 단일 원본(Source of Truth)** 으로 사용합니다.
UI에서는 보드 형태로 편집하고, 저장 시 다시 위키 텍스트로 직렬화합니다.

---

## 1) 카드 문법

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

### 구성 요소
- `=== 리스트명`: 컬럼
- `==== 카드 제목 ==== #cardId`: 카드(안정 ID 필수)
- `===== Property`: 속성 블록
- `===== Activity`: 활동 로그 블록

### 제약 조건
- 카드 ID(`#...`)는 생성 후 변경하지 않음
- `Assignee` 키 표기 고정
- 날짜 포맷은 `YYYY-MM-DD`

---

## 2) 저장/동기화 동작

### API
- 저장: `POST /w/:pageName`
- 최신 리비전 조회: `GET /api/pageRevision/:pageName`
- 코멘트 렌더링: `POST /api/renderAhaMark/:pageName`

### 저장 규칙
- Kanban 블록 외부 텍스트는 변경하지 않음
- 카드/리스트 순서는 입력 순서를 그대로 유지
- 저장 전 현재 리비전이 확인되지 않으면 최신 리비전을 1회 조회한 뒤 저장
- `409 Conflict` 발생 시 사용자에게 알리고 최신 페이지로 새로고침

### 카드 모달 URL 동기화
- 열기 시 URL hash를 `#<cardId>`로 설정
- 닫기 시 해당 hash 제거
- `#<cardId>`로 직접 진입하면 해당 카드 모달을 자동으로 열고 보드 위치로 이동
- 존재하지 않는 `#<cardId>`는 오류 없이 일반 보드 화면을 유지

---

## 3) 리비전 코멘트 규칙

### 공통 포맷
- 접두어: `Kanban - <EventPrefix> - ...`

### 지원 액션
- 리스트: `list:add`, `list:rename`, `list:move`, `list:delete`
- 카드: `card:add`, `card:rename`, `card:move`, `card:delete`
- 카드 상세: `card:comment:add`, `card:property:update`

### 세부 규칙
- `card:comment:add`: 코멘트 첫 줄 기준 최대 80자(초과 시 `...`)
- `card:property:update` 값 직렬화
  - 단일값: 문자열
  - 다중값: `, `로 join
  - 빈 값: `(empty)`
- `card:delete`: 카드가 삭제되므로 Activity에는 기록하지 않음

---

## 4) Property 허용 키

- `Creator` (1)
- `DueDate` (1)
- `Assignee` (n)
- `Attachment` (n)

---

## 5) 자동 테스트

- `test/kanban.roundtrip-fixtures.test.mjs`
- `test/kanban.revision-comment.test.mjs`
- `test/kanban.retry-409.test.mjs`
- `test/kanban.hash-navigation.test.mjs`

검증 결과: **완료 판정 가능**

- 검증일: 2026-05-22
- 명령: `node --test test\kanban*.test.mjs`
- 결과: 25개 테스트 통과

