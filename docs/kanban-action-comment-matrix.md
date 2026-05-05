# Kanban 액션별 코멘트 매트릭스

아래 표는 현재 코드 기준으로, Kanban에서 발생하는 주요 액션에 대해
- **리비전 코멘트**(페이지 히스토리에 남는 `Page.comment`)
- **위키 본문 코멘트/Activity**(카드 본문 `===== Activity` 아래에 남는 줄)
를 정리한 것입니다.

## 공통 포맷

- 리비전 코멘트 접두어: `Kanban - `
- 사용자 친화 포맷(지원 액션): `Kanban - <EventPrefix> - ...`
- 비지원 액션/메타 누락 시 fallback: `Kanban - kanban:save ...`

## 액션별 매핑

| 액션 타입 | 리비전 코멘트 (`Page.comment`) | 위키 본문(Activity) 변화 |
|---|---|---|
| `list:add` | `Kanban - <EventPrefix> - Added a List - '''<listTitle>'''` | 없음 |
| `list:rename` | `Kanban - <EventPrefix> - Renamed List Title - '''<fromTitle>''' to '''<toTitle>'''` | 없음 |
| `list:move` | `Kanban - <EventPrefix> - Moved a List '''<listTitle>''' Order - ["#<fromOrder>"] to ["#<toOrder>"]` | 없음 |
| `list:delete` | `Kanban - <EventPrefix> - Deleted a List - '''<listTitle>'''` | 없음 |
| `card:add` | `Kanban - <EventPrefix> - Added a Card - ["#<cardId>" <cardTitle>]` | 카드 생성 시 Activity 초기값 추가:<br>`* <creationComment or "Created card">`<br>`  * Created card` |
| `card:rename` | `Kanban - <EventPrefix> - Renamed Card Title - ["#<cardId>" <fromTitle>] to ["#<cardId>" <toTitle>]` | 없음 |
| `card:move` (리스트 간 이동) | `Kanban - <EventPrefix> - Moved a Card ["#<cardId>" <cardTitle>] - '''<fromList>''' to '''<toList>'''` | Activity에 새 엔트리 1개 추가:<br>`* [User:<author>] [<date>]T<time>`<br>`  * Moved from '''<fromList>''' to '''<toList>'''` |
| `card:move` (같은 리스트 내 순서 이동) | `Kanban - <EventPrefix> - Moved a Card ["#<cardId>" <cardTitle>] Order - ["#<fromOrder>"] to ["#<toOrder>"]` | 없음 |
| `card:delete` | `Kanban - <EventPrefix> - Deleted a Card - ["#<cardId>" <cardTitle>]` | 카드 자체가 삭제되어 Activity도 함께 제거 |
| `card:comment:add` | `Kanban - <EventPrefix> - Added a comment on ["#<cardId>" <cardTitle>] - <comment>` | Activity에 새 엔트리 1개 추가:<br>`* [User:<author>] [<date>]T<time>`<br>`  * <comment>` |

## 추가 매핑

| 액션 타입 | 리비전 코멘트 | 위키 본문(Activity) 변화 |
|---|---|---|
| `card:property:update` | `Kanban - <EventPrefix> - Updated Card Property - ["#<cardId>" <cardTitle>] <property>=<value>` | DueDate 변경 시 Activity에 새 엔트리 추가:<br>`* [User:<author>] [<date>]T<time>`<br>`  * Updated DueDate`<br>`  * <old> to <new>` |

## 참고

- 표의 `<EventPrefix>`는 서버의 `eventPrefix` 값(예: 사용자/행위 prefix)입니다.
- Activity 엔트리의 header는 JS에서 `[User:<author>] [YYYY-MM-DD]THH:mm:ssZ` 형태로 저장되고, 클라이언트 표시 시 timezone 변환되어 보일 수 있습니다.
