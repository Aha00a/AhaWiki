# 위키 텍스트 기반 Kanban 기능 설계안 (현재 기준)

사용자 의도: **위키 문법(불릿 기반)을 원본 데이터로 사용**하고, 화면은 **Kanban 보드 UI**로 렌더링하며, 드래그 앤 드롭 결과를 다시 **위키 텍스트로 저장**합니다.

---

## 1) 결론 (좋은 방향인가?)
네, 아주 좋은 방향입니다.

- 기존 위키 편집 경험을 유지합니다.
- 구조화된 데이터베이스 없이도 Kanban을 도입할 수 있습니다.
- "텍스트가 진실의 원천(source of truth)"이므로 백업/이력/비교가 쉽습니다.
- UI는 시각적으로 편하고, 저장은 단순 텍스트라 구현 리스크가 낮습니다.

핵심은 **문법 제약을 명확히 정의**하고, **파서-렌더러-역직렬화(serializer)**를 안정적으로 만드는 것입니다.

---

## 2) 제안 문법 (현행)
이제 Kanban 카드는 **구조형 카드 포맷만 사용**합니다.

### 구조형 카드 포맷 (기본)

```wiki
=== Info
==== 3차 정보 ==== #mpax1u9pqv5e6wta
===== Property
 * DueDate: [2027-01-01]
 * Assignee
  * [User:Aha00a]
 * Attachment
  * [[Attachment(clipboard/clipboard.2026-05-04T20-49-15.png)]]
  * [[Attachment(clipboard/clipboard.2026-05-04T20-54-56.png)]]
===== Activity
 * [User:Aha00a] [2026-05-04]T10:29:55
 * [User:Aha00a] [2026-05-04]T10:36:17
  * Moved from '''Done''' to '''Info'''
```

해석 규칙:
- `=== 리스트명` → Kanban **리스트(Column)**
- `==== 카드 제목 ==== #cardId` → **카드 헤더(제목 + 안정 ID)**
- `===== Property` → 구조 데이터(마감일/담당자/첨부 등)
- `===== Activity` → 활동 로그(append-only 이벤트)

공통 제약(권장):
- 카드 ID(`#...`)는 생성 후 유지.
- `Assignee` 표기 통일(`Asignee` 금지).
- 날짜 포맷 고정: `YYYY-MM-DD` (예: `2027-01-01`).

---

## 3) 내부 데이터 모델 (메모리)
렌더링/드래그 처리용 중간 모델:

- Board
  - columns: Column[]
- Column
  - id: string (안정 ID)
  - title: string
  - cards: Card[]
- Card
  - id: string (안정 ID)
  - title: string
  - property: Map[String, Seq[String]]
  - activity: Activity[]
- Activity
  - actor: string
  - timestamp: string
  - lines: Seq[String]

ID 전략(중요):
- 모든 카드는 `#cardId`를 최우선 식별자로 사용합니다.
- 카드 생성 시 신규 `#cardId`를 반드시 부여하고 이후 유지합니다.

---

## 4) 렌더링 규칙
1. 문서에서 Kanban 대상 블록을 파싱.
2. 컬럼/카드 모델 생성.
3. 보드 컴포넌트로 렌더링.
4. 카드 drag & drop 지원:
   - 같은 컬럼 내 순서 변경
   - 다른 컬럼으로 이동

UI 동작 권장:
- `Property`는 폼 위젯(DueDate/Assignee/Attachment), `Activity`는 로그 타임라인으로 표시.
- 저장 전 "위키 텍스트 미리보기" 제공.
- 충돌 시(동시 편집) 머지 안내.

---

## 5) 저장(역직렬화) 규칙
드래그 결과 모델 → 위키 텍스트로 재생성:

- 리스트 순서/제목 유지
- 카드 순서 유지
- 구조형 카드 포맷으로 직렬화

구조형 카드 직렬화:
- `==== 카드 제목 ==== #cardId`
- `===== Property`
- ` * Key: Value` 또는 ` * Key` + 하위 값 불릿
- `===== Activity`
- 기존 로그 라인 보존(append-only)

저장 시 주의점:
- Kanban 블록 외부 텍스트는 절대 훼손하지 않기.
- 줄바꿈/공백 스타일을 최대한 원문과 동일하게 유지.
- 실패 시 원문 롤백 가능해야 함.

---

## 6) 파서 구현 전략
안전한 방식은 "전체 위키 파서"보다 "Kanban 블록 제한 파서"입니다.

권장 절차:
1. 문서에서 Kanban 블록 범위 탐지.
2. 해당 범위만 라인 단위로 파싱.
3. 구조형 카드 규칙으로 파싱.
4. 규칙 위반 라인은 `raw`로 보관.
5. 저장 시 `raw` 라인은 보존 또는 하단 재배치.

이 방식이 좋은 이유:
- 기존 위키 문법 전체를 깨지 않음.
- 기능 범위를 작게 잡아 버그를 줄임.
- 카드 문법을 단일화해 파싱 복잡도를 낮춤.

---

## 7) MVP 범위 제안 (현행)
1차(MVP)
- 리스트/카드 렌더링(구조형)
- 카드 이동(컬럼 간/내)
- 위키 텍스트 저장(원문 훼손 최소화)

2차
- 구조형 `Property` 편집 UI(DueDate/Assignee/Attachment)
- `Activity` append-only 로깅
- 새 카드/컬럼 추가

3차
- 실시간 동시편집 충돌 처리
- 권한/감사 로그 강화

---

## 8) 액션별 리비전 코멘트/Activity 매트릭스 (통합)

아래 표는 현재 코드 기준으로, Kanban에서 발생하는 주요 액션에 대해
- **리비전 코멘트**(페이지 히스토리에 남는 `Page.comment`)
- **위키 본문 코멘트/Activity**(카드 본문 `===== Activity` 아래에 남는 줄)
를 정리한 것입니다.

### 공통 포맷

- 리비전 코멘트 접두어: `Kanban - `
- 사용자 친화 포맷(지원 액션): `Kanban - <EventPrefix> - ...`
- 비지원 액션/메타 누락 시 fallback: `Kanban - kanban:save ...`

### 액션별 매핑

| action type                 | `Page.comment`                                                                                              |
|-----------------------------|-------------------------------------------------------------------------------------------------------------|
| `list:add`                  | `Kanban - <EventPrefix> - List Add - '''<listTitle>'''`                                                     |
| `list:rename`               | `Kanban - <EventPrefix> - List Rename - '''<fromTitle>''' to '''<toTitle>'''`                               |
| `list:move`                 | `Kanban - <EventPrefix> - List Move - '''<listTitle>''' - ["#<fromOrder>"] to ["#<toOrder>"]`               |
| `list:delete`               | `Kanban - <EventPrefix> - List Delete - '''<listTitle>'''`                                                  |
| `card:add`                  | `Kanban - <EventPrefix> - Card Add - ["#<cardId>" <cardTitle>]`                                             |
| `card:rename`               | `Kanban - <EventPrefix> - Card Rename - ["#<cardId>" <fromTitle>] to ["#<cardId>" <toTitle>]`               |
| `card:move` (to other list) | `Kanban - <EventPrefix> - Card Move - ["#<cardId>" <cardTitle>] - '''<fromList>''' to '''<toList>'''`       |
| `card:move` (in same list)  | `Kanban - <EventPrefix> - Card Move - ["#<cardId>" <cardTitle>] Order - ["#<fromOrder>"] to ["#<toOrder>"]` |
| `card:delete`               | `Kanban - <EventPrefix> - Card Deleted - ["#<cardId>" <cardTitle>]`                                         |
| `card:comment:add`          | `Kanban - <EventPrefix> - Card Comment Add - ["#<cardId>" <cardTitle>] - <comment>`                         |
| `card:property:update`      | `Kanban - <EventPrefix> - Card Property Update - ["#<cardId>" <cardTitle>] - <property> - <value>`          |

Card에 관한 Action이 이루어질 경우 Activity 맨 처음에
```
 * `[User:<author>] [<date>]T<time>`
  * <comment> 
```
를 삽입해준다. <comment>는 위 표의 Page.comment 메시지에서 `Kanban - <EventPrefix> - `를 지우고 뒷부분만 넣어준다. 

### 참고

- 표의 `<EventPrefix>`는 서버의 `eventPrefix` 값(예: 사용자/행위 prefix)입니다.
- Activity 엔트리의 header는 JS에서 `[User:<author>] [YYYY-MM-DD]THH:mm:ssZ` 형태로 저장되고, 클라이언트 표시 시 timezone 변환되어 보일 수 있습니다.

---

## 9) 기대 효과
- 활동 로그와 업무 속성의 책임 분리
- 파싱 안정성/자동화 용이성 향상
- 저장 diff 가독성 향상

즉, **UI는 Kanban**, **저장은 위키 텍스트**라는 목표를 유지하면서,
실무에 필요한 구조화 메타데이터까지 유연하게 다룰 수 있습니다.

---

## 10) 간소화 이관 체크리스트 (미사용 기능 기준)

현재 Kanban은 **실사용 기능이 아니므로**, 안전장치/장기 병행 없이 빠르게 **페이지 저장(`POST /w/:pageName`) 단일 경로**로 이관합니다.

### Fast Track — 1회성 정리
- [x] 클라이언트에서 리스트/카드 생성 포함 모든 변경을 `persistColumns(actionType, actionMeta)` + `POST /w/:pageName` 저장으로 통일
- [x] `/api/Kanban/:pageName/list`, `/api/Kanban/:pageName/card` 호출 코드 제거
- [x] 서버 라우트에서 `/api/Kanban/*` 엔드포인트 제거
- [x] 문서의 API 예시/설명을 `POST /w/:pageName`/`/api/pageRevision`/`/api/renderAhaMark` 기준으로 정리

#### 현재 API 기준 (최신)
- 저장: `POST /w/:pageName`
  - 주요 필드: `revision`, `text`, `comment`, `minorEdit`, `recaptcha`, `lineStart`, `lineEnd`
- 최신 리비전 조회: `GET /api/pageRevision/:pageName`
- 코멘트 렌더링: `POST /api/renderAhaMark/:pageName`

### 구현 기준
- [x] `actionType/actionMeta`는 기존 키를 최대한 재사용하고, 누락 시 fallback comment 허용
- [x] 실패 처리 정책은 단순화(실패 알림 + 사용자 재시도)하고 자동 복구/롤백 로직은 두지 않음
- [x] 성능/운영 지표, 단계적 deprecate 공지, 장기 병행 운영은 생략

### 완료 기준 (Done)
- [x] Kanban 변경 요청이 `POST /w/:pageName`로만 전송됨
- [x] `/list`, `/card` 관련 코드/라우트는 저장소에서 제거됨
- [x] 기본 동작(리스트 추가, 카드 추가, 이동, 제목/속성/코멘트 저장) 수동 확인 완료

#### 수동 확인 체크리스트 (최종)
- [x] 리스트 추가 후 페이지 히스토리에 새 revision이 생성된다.
- [x] 카드 추가 후 카드 ID/Property/Activity가 직렬화되어 저장된다.
- [x] 카드 순서 이동(같은 리스트) 후 새로고침 시 순서가 유지된다.
- [x] 카드 이동(리스트 간) 후 새로고침 시 위치가 유지된다.
- [x] 카드 제목 수정/삭제가 저장 후 그대로 반영된다.
- [x] 카드 DueDate/속성 변경이 저장 후 그대로 반영된다.
- [x] 카드 코멘트 추가(텍스트/클립보드 이미지)가 저장 후 그대로 반영된다.
- [x] 충돌 상황(다른 탭에서 먼저 저장)에서 409 재시도 후 저장이 정상 완료된다.
