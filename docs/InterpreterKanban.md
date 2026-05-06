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
