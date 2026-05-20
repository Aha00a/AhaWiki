# TODO: Timezone KST to UTC Migration

## Goal

현재 서버와 DB가 `Asia/Seoul` 기준으로 동작하고 있고, DB의 `DATETIME` 값도 KST 로컬 시각으로 저장되어 있다.

장기 목표는 다음과 같다.

- 서버/JVM 기준 타임존을 UTC로 변경한다.
- DB 기준 타임존을 UTC로 변경한다.
- timestamp 성격의 DB 값은 UTC 기준으로 저장한다.
- API/HTML/WebSocket으로 내려보낼 때는 `Z` 또는 offset이 포함된 ISO-8601 문자열을 사용한다.
- 화면 표시 시에는 브라우저에서 사용자 타임존 기준으로 변환한다.

## Current Assumption

기존 DB의 타임스탬프성 `DATETIME` 컬럼 값은 KST 의미로 해석한다.

예:

```text
DB value: 2026-05-21 01:30:00
Meaning : 2026-05-21 01:30:00 Asia/Seoul
UTC     : 2026-05-20 16:30:00Z
```

따라서 기존 `DATETIME` 값을 UTC 기준으로 정리하려면 `-9 hours` 변환이 필요하다.

## MySQL Type Notes

MySQL에는 PostgreSQL의 `timestamptz` 같은 "타임존 포함 datetime" 타입이 없다.

- `DATETIME`: 타임존 없이 입력된 날짜/시간을 그대로 저장한다.
- `TIMESTAMP`: 내부적으로 UTC 기준 저장 및 session `time_zone` 기준 변환을 수행하지만, 원래 타임존 이름이나 offset을 값에 저장하지 않는다.
- 사용자 타임존 자체가 비즈니스 의미를 가지면 별도 컬럼에 저장해야 한다.

현재 코드베이스는 `DATETIME`/`LocalDateTime` 중심이므로, 우선은 `DATETIME`을 유지하되 값의 의미를 UTC로 고정하는 방향이 현실적이다.

## Migration Strategy

### 1. Inventory datetime columns

운영 DB에서 timestamp 성격의 컬럼을 전수 확인한다.

```sql
SELECT TABLE_NAME, COLUMN_NAME, DATA_TYPE
FROM INFORMATION_SCHEMA.COLUMNS
WHERE TABLE_SCHEMA = DATABASE()
  AND DATA_TYPE IN ('datetime', 'timestamp')
ORDER BY TABLE_NAME, COLUMN_NAME;
```

주의:

- `DATETIME` 컬럼은 기존 KST 로컬 값을 UTC 로컬 값으로 변환해야 한다.
- `TIMESTAMP` 컬럼은 session timezone 변환이 개입되므로 별도로 확인한다.
- 날짜 자체가 콘텐츠 의미인 값은 변환하지 않는다.

### 2. Identify application touch points

우선 확인할 코드 위치:

- `app/com/aha00a/play/AnormSqlParser.scala`
  - DB 값을 `LocalDateTime`으로 읽는다.
- `app/com/aha00a/commons/Implicits.scala`
  - `toIsoLocalDateTimeString`이 offset 없는 문자열을 만든다.
- `app/assets/js/admin.jsx`
  - `dayjs(value)`가 offset 없는 값을 브라우저 로컬 시간으로 파싱할 수 있다.
- Twirl views
  - history, action, blame, search, RecentChanges 등에서 서버 시각 문자열을 직접 출력한다.
- API/WebSocket 응답
  - `dateTime`, `created`, `updated`, `dateInserted`, `dateUpdated` 등 timestamp 값을 문자열로 내려준다.

### 3. Add UTC/ISO formatting helpers

서버에서 timestamp를 외부로 내보낼 때는 offset 없는 문자열을 피한다.

권장 출력:

```text
2026-05-20T16:30:00Z
```

또는:

```text
2026-05-21T01:30:00+09:00
```

UTC 전환 이후에는 `Z` 형식을 기본으로 삼는다.

### 4. Update frontend formatters

브라우저에서는 offset 포함 ISO 문자열만 받아서 사용자 타임존으로 표시한다.

예:

```js
function formatDateTimeInClientTimezone(value) {
  if (!value) return "-";
  const date = new Date(value);
  if (Number.isNaN(date.getTime())) return value;
  return new Intl.DateTimeFormat(undefined, {
    year: "numeric",
    month: "2-digit",
    day: "2-digit",
    hour: "2-digit",
    minute: "2-digit",
    second: "2-digit",
    hour12: false,
  }).format(date);
}
```

주의:

- `2026-05-21T01:30:00`처럼 offset 없는 값은 브라우저 로컬 시간으로 해석될 수 있다.
- API 응답부터 `2026-05-20T16:30:00Z`처럼 명확한 값을 내려야 한다.

### 5. Plan maintenance window

운영 반영 시 권장 순서:

1. 점검 모드 또는 쓰기 중단
2. DB 백업
3. timestamp 성격의 기존 `DATETIME` 값을 `-9 hours` 변환
4. DB timezone을 UTC로 고정
5. JVM timezone을 UTC로 변경
6. 앱 배포
7. 신규 저장값과 화면 표시값 검증

### 6. Convert existing KST DATETIME data

예시:

```sql
UPDATE Page
SET dateTime = DATE_SUB(dateTime, INTERVAL 9 HOUR);
```

컬럼별로 동일한 변환을 적용한다.

예상 후보:

- `Page.dateTime`
- `PageMeta.dateInserted`
- `PageMeta.dateUpdated`
- `User.created`
- `User.updated`
- `UserSite.created`
- `SiteDomain.created`
- `Attachment.dateInserted`
- `Attachment.dateUpdated`
- `AccessLog.dateInserted`
- `UserViewHistory.dateInserted`
- `CacheCrawler.dateInserted`
- `CacheCrawler.dateUpdated`
- `Config.created`
- `Config.updated`
- `IpDeny.dateInserted`
- `CalculatedTerm.dateInserted`
- `GeocodeCache.created`

실제 컬럼 목록은 운영 DB의 `INFORMATION_SCHEMA` 결과로 확정한다.

### 7. Change runtime timezone settings

현재 start script에는 KST 설정이 있다.

```text
-Duser.timezone=Asia/Seoul
```

UTC 전환 후:

```text
-Duser.timezone=UTC
```

DB도 global/session timezone이 UTC가 되도록 설정한다.

```sql
SET GLOBAL time_zone = '+00:00';
SET SESSION time_zone = '+00:00';
```

운영에서는 DB 설정 파일 또는 connection initialization에서 session timezone을 고정하는 방식을 확인한다.

## Exclusions

다음은 타임존 변환 대상이 아니다.

- 위키 페이지명으로 쓰이는 날짜
  - 예: `2026-05-21`
- 달력/일지/월별 페이지명
- 문서 내용 안에서 사용자가 명시적으로 적은 날짜
- 날짜 자체가 비즈니스 의미인 값

timestamp 성격의 값만 변환한다.

## Verification Checklist

- [ ] 운영 DB 백업을 완료했다.
- [ ] timestamp 성격의 `DATETIME`/`TIMESTAMP` 컬럼 목록을 확정했다.
- [ ] 변환 대상과 제외 대상을 구분했다.
- [ ] staging DB에서 `-9 hours` 변환을 리허설했다.
- [ ] JVM timezone을 UTC로 바꾼 상태에서 신규 저장 시간이 UTC로 들어가는지 확인했다.
- [ ] DB session timezone이 UTC인지 확인했다.
- [ ] API 응답 시간이 `Z` 또는 offset 포함 ISO-8601인지 확인했다.
- [ ] WebSocket payload 시간이 `Z` 또는 offset 포함 ISO-8601인지 확인했다.
- [ ] history/action/blame/search/RecentChanges 화면이 사용자 타임존으로 표시되는지 확인했다.
- [ ] admin 화면의 날짜 formatter가 offset 없는 문자열에 의존하지 않는지 확인했다.
- [ ] KST 사용자에게 기존 표시 시각이 마이그레이션 전과 동일하게 보이는지 확인했다.
- [ ] KST가 아닌 사용자에게 브라우저 타임존 기준으로 표시되는지 확인했다.

## Rollback Notes

문제가 발생했을 때는 DB 백업 복구가 가장 안전하다.

단순히 JVM/DB timezone만 KST로 되돌리면, 이미 `-9 hours` 변환된 데이터와 설정이 어긋날 수 있다.

롤백 계획은 다음 중 하나로 명확히 잡는다.

- 백업 복구
- 변환 SQL의 역연산 적용

역연산 예:

```sql
UPDATE Page
SET dateTime = DATE_ADD(dateTime, INTERVAL 9 HOUR);
```

역연산 방식은 모든 변환 대상 컬럼에 빠짐없이 적용해야 하므로, 운영에서는 백업 복구가 더 안전할 수 있다.
