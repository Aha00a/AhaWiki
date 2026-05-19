# Permission

이 문서는 현재 코드에 구성되어 있는 사용자 Permission 구조와 앞으로 정리해야 할 작업을 정리한다.

## 요약

현재 위키 페이지 접근 제어는 `Permission` 테이블만 사용한다.

- 실제 읽기/쓰기 권한 판정: `logics.wikis.WikiPermission`
- 권한 데이터 소스: `Permission` 테이블
- legacy 권한 데이터: `.config`, `#!read`, `#!write`, `Page.permRead`, `PageMeta.permRead`
- 현재 정책: legacy 권한 데이터는 실제 접근 제어에 사용하지 않는다.
- 매칭되는 `Permission` row가 없으면 기본적으로 denied 처리된다.

따라서 공개 읽기 같은 기본 정책도 반드시 `Permission` 테이블 row로 표현해야 한다.

예를 들어 사이트 전체를 공개 읽기로 만들려면 다음과 같은 row가 필요하다.

```sql
INSERT INTO Permission (site, target, targetType, actor, actorType, action)
VALUES (1, '', 'All', '', 'All', 1);
```

로그인 사용자에게 전체 편집/생성을 허용하려면 다음과 같은 row가 필요하다.

```sql
INSERT INTO Permission (site, target, targetType, actor, actorType, action)
VALUES (1, '', 'All', '', 'Login', 4);
```

## 로그인 사용자

로그인은 Google OAuth 기반이다.

1. `GoogleOAuth.callback`에서 Google profile을 조회한다.
2. `User.selectOrInsert(email, profileImageUrl)`로 사용자를 찾거나 생성한다.
3. `SessionLogic.login`이 session에 다음 값을 저장한다.
   - `seq`
   - `email`
   - `nickname`
   - `profileImageUrl` optional

요청 중 현재 사용자는 `RequestWrapper.getUser`를 통해 `SessionLogic.getUser(request)` 결과로 접근한다.

세션에 `seq`, `email`, `nickname` 중 하나라도 없으면 비로그인 사용자로 취급된다. `Permission` 판정에서 비로그인 사용자의 actor 값은 빈 문자열 `""`이다.

관리자 판정은 아직 별도 role 테이블이 아니라 코드 조건으로 남아 있다.

```scala
SessionLogic.getUser(request).exists(u => u.email == "aha00a@gmail.com" || u.seq == 1)
```

## WikiPermission

실제 페이지 읽기/쓰기 권한 판정은 `logics.wikis.WikiPermission`이 담당한다.

현재 판정 순서:

1. 현재 사이트의 `Permission` rows를 조회한다.
2. 요청 page name과 actor email에 매칭되는 row를 찾는다.
3. 가장 구체적인 row 하나를 선택한다.
4. `row.action >= requiredAction`이면 허용한다.
5. 매칭되는 row가 없으면 거부한다.

중요한 점:

- 더 이상 `#!read`, `#!write`, `.config`의 `permission.default.*`로 fallback하지 않는다.
- 더 이상 `Page.permRead`, `PageMeta.permRead`를 읽기 권한 필터링에 사용하지 않는다.
- `Permission` row가 없는 page/actor 조합은 denied다.

## Required Action

현재 action 값은 bit flag가 아니라 등급처럼 동작한다.

`Permission.permitted(action)`은 다음처럼 판정한다.

```scala
this.action >= action
```

모델의 action 값:

| 이름 | 값 |
| --- | ---: |
| `none` | 0 |
| `read` | 1 |
| `edit` | 2 |
| `create` | 4 |
| `upload` | 8 |
| `delete` | 16 |
| `admin` | 255 |

현재 주요 사용:

- 기존 페이지 읽기: `read`
- 기존 페이지 편집/저장: `edit`
- 없는 페이지 생성: `create`
- signed read URL 생성 등 관리자성 기능: 별도 hard-coded admin 판정도 함께 존재

## Permission 테이블

`Permission` 테이블은 evolution 18에서 추가되었고, evolution 50에서 `targetType`, `actorType`, `dateInserted`, `dateUpdated`가 추가되었다.

현재 주요 컬럼:

- `site`
- `target`
- `targetType`
- `actor`
- `actorType`
- `action`
- `dateInserted`
- `dateUpdated`

현재 primary key:

```text
(site, targetType, target, actorType, actor)
```

### Target

`target`은 page name에 매칭된다.

| targetType | target 예 | 의미 |
| --- | --- | --- |
| `All` | `''` | 모든 페이지 |
| `Exact` | `Private` | 정확히 `Private` 페이지 |
| `StartsWith` | `Private` | `Private`로 시작하는 모든 페이지 |
| `EndsWith` | `/Secret` | `/Secret`으로 끝나는 모든 페이지 |

신규 코드와 SQL에서는 `targetType`을 명시해야 한다.

### Actor

`actor`는 현재 사용자 email에 매칭된다.

| actorType | actor 예 | 의미 |
| --- | --- | --- |
| `All` | `''` | 비로그인 포함 모든 사용자 |
| `Login` | `''` | 로그인한 모든 사용자 |
| `Exact` | `user@example.com` | 해당 email 사용자 |
| `Domain` | `@example.com` | 해당 domain으로 끝나는 email 사용자 |

## 우선순위

`PermissionLogic`은 permission 목록을 다음 기준으로 정렬한 뒤 처음 매칭되는 row를 사용한다.

1. `specificity` 내림차순
2. `target.length` 내림차순
3. `actor.length` 내림차순

`specificity`는 `targetLevel + actorLevel`이다.

target level:

- `All`: 1
- `StartsWith`, `EndsWith`: 2
- `Exact`: 3

actor level:

- `All`: 1
- `Login`, `Domain`: 2
- `Exact`: 3

따라서 일반적으로 exact target + exact actor가 가장 먼저 적용된다.

## 읽기 권한 적용 지점

읽기 권한은 `WikiPermission.isReadable` / `isReadableByAnonymous`를 통해 판정한다.

주요 적용 지점:

- `Wiki.view`
- `raw`
- `history`
- `blame`
- `diff`
- 검색 결과 필터링
- page list 필터링
- include macro
- WebSocket watch
- API 일부 응답 필터링

읽기가 거부되면 일반적으로 `Permission denied.`가 반환된다.

signed read URL은 예외적으로 유효한 서명이 있으면 페이지를 읽을 수 있다. signed read URL은 `sr_exp`, `sr_sig` query parameter와 `play.http.secret.key` 기반 HMAC으로 검증된다.

## 쓰기 권한 적용 지점

쓰기 권한은 `WikiPermission.isWritable`로 판정한다.

현재 페이지 존재 여부에 따라 required action이 다르다.

- 기존 페이지: `edit`
- 없는 페이지: `create`

주요 적용 지점:

- 없는 페이지의 `edit`
- 기존 페이지의 `edit`
- save
- `rename`
- `delete`
- attachment upload/delete 관련 일부 경로
- diary write
- Google Spreadsheet sync

save 시에는 form body의 새 본문이 아니라 DB의 최신 본문을 기준으로 현재 page가 존재하는지 판단하고 required action을 계산한다.

## 목록/검색/매크로 권한 필터링

페이지 목록 권한 필터링은 `PageLogic.getListPageByPermission`에서 수행된다.

현재 흐름:

1. `PageMeta` 기반 최신 페이지 목록을 가져온다.
2. 각 page name에 대해 `WikiPermission.isReadable(pageName)`을 호출한다.
3. `Permission` 테이블 기준으로 허용된 페이지만 남긴다.

`PageMeta.permRead`는 더 이상 필터링에 사용하지 않는다.

이 결과는 다음 필드로 노출된다.

- `ContextSite.seqPageByPermission`
- `ContextSite.seqPageNameByPermission`
- `ContextSite.setPageNameByPermission`

검색 결과도 각 결과의 page name에 대해 `WikiPermission.isReadable`을 다시 적용한다.

## Legacy Permission 데이터

아래 값들은 과거 shebang/default 기반 권한 체계에서 사용되었다.

- `.config`의 `permission.default.read`
- `.config`의 `permission.default.write`
- 페이지 상단 `#!read ...`
- 페이지 상단 `#!write ...`
- `Page.permRead`
- `PageMeta.permRead`

현재 실제 권한 판정에서는 사용하지 않는다.

현재 코드 상태:

- `PageContent`는 `read` / `write` directive를 권한 값으로 노출하지 않는다.
- `PageContent`는 기존 문서 렌더링 호환성을 위해 `read` / `write` directive를 interpreter shebang에서는 계속 제외한다.
- `Page` / `PageMeta` 모델에서는 `permRead` 필드가 제거되었다.
- evolution 51에서 `Page.permRead`, `PageMeta.permRead` 컬럼을 제거한다.

즉, legacy 데이터는 권한 소스로 사용하지 않으며 schema에서도 제거 대상이다.

## Flash 표시

읽기 제한이 있는 페이지에서는 상단 flash에 `Permission` 테이블 기준 상세 정보가 표시된다.

표시 내용:

- 현재 사용자
- 현재 사용자 read 허용 여부와 매칭 row
- anonymous read 허용 여부와 매칭 row
- 현재 사용자 write 허용 여부와 매칭 row

이 flash 역시 shebang/default 기준 설명은 표시하지 않는다.

## 예시

모든 사용자에게 전체 페이지 읽기 허용:

```sql
INSERT INTO Permission (site, target, targetType, actor, actorType, action)
VALUES (1, '', 'All', '', 'All', 1);
```

로그인 사용자에게 전체 페이지 생성/편집 허용:

```sql
INSERT INTO Permission (site, target, targetType, actor, actorType, action)
VALUES (1, '', 'All', '', 'Login', 4);
```

특정 사용자에게 전체 관리자 권한 부여:

```sql
INSERT INTO Permission (site, target, targetType, actor, actorType, action)
VALUES (1, '', 'All', 'aha00a@gmail.com', 'Exact', 255);
```

`Private` prefix 페이지를 기본 차단:

```sql
INSERT INTO Permission (site, target, targetType, actor, actorType, action)
VALUES (1, 'Private', 'StartsWith', '', 'All', 0);
```

`Private` prefix 페이지를 특정 사용자에게 허용:

```sql
INSERT INTO Permission (site, target, targetType, actor, actorType, action)
VALUES (1, 'Private', 'StartsWith', 'aha00a@gmail.com', 'Exact', 255);
```

특정 domain 사용자에게 특정 prefix 읽기 허용:

```sql
INSERT INTO Permission (site, target, targetType, actor, actorType, action)
VALUES (1, 'Team', 'StartsWith', '@example.com', 'Domain', 1);
```

## 완료 기준

Permission 정리는 다음 상태가 되면 완료로 본다.

- 실제 접근 제어 경로가 모두 `Permission` 테이블을 기준으로 동작한다.
- 공개 읽기, 로그인 편집, private prefix 예외 같은 운영 정책이 모두 명시적인 `Permission` row로 표현된다.
- `.config`, `#!read`, `#!write`, `Page.permRead`, `PageMeta.permRead`는 권한 소스가 아니라 legacy 데이터로만 남거나 제거된다.
- 운영자가 특정 page/user 조합에 대해 어떤 row가 매칭되는지 확인할 수 있다.
- 매칭 row 없음, target/actor 우선순위, 목록/검색/include 필터링을 테스트로 보장한다.

## 남은 작업

### 운영 데이터

- [x] 운영 DB에서 기존 `.config`, `#!read`, `#!write`, `Page.permRead`, `PageMeta.permRead` 기준 정책을 `Permission` row로 이관한다.
- [x] 모든 사이트에 최소 기본 정책 row가 존재하는지 점검한다.
- [x] `Permission` row가 없어서 의도치 않게 닫히는 페이지가 없는지 확인하는 점검 쿼리나 dev endpoint를 만든다.

### 코드 정리

- [x] `PageContent.read`, `PageContent.write` 사용처를 확인하고 권한 목적 사용을 완전히 제거한다.
- [x] 더 이상 권한에 쓰지 않는 `permission.default.read/write` 설정을 제거하거나 deprecated 문서로 분리한다.
- [x] `Page.permRead`, `PageMeta.permRead` 컬럼 제거를 위한 evolution을 준비한다.
- [x] `Page`, `PageWithoutContent`, `PageWithoutContentWithSize`, `PageMeta` 모델에서 `permRead` 필드를 제거한다.
- [x] admin PageMeta 화면에서 `permRead` 표시가 있다면 제거하거나 legacy 표시로 명확히 이름을 바꾼다.
- [x] `Test.permission` endpoint를 DB-only 진단 도구로 다시 설계한다.

### 모델 개선

- [x] `All`, `Exact`, `StartsWith`, `EndsWith` targetType 매칭 단위 테스트를 추가한다.
- [x] `All`, `Login`, `Domain`, `Exact` actorType 매칭 단위 테스트를 추가한다.
- [x] 기본적인 `PermissionLogic` 우선순위 단위 테스트를 추가한다.
- [x] `action`을 Int 상수 대신 enum 또는 ADT로 정리한다.
- [x] `action >= requiredAction` 등급 모델이 실제 정책에 충분한지 재검토한다.
- [x] `create`, `edit`, `delete`, `upload`, `admin`의 관계가 등급형으로 맞는지 테스트를 보강한다.
- [x] `Permission.apply(target, actor, action)` legacy 추론 helper를 제거하거나 테스트 전용으로 격리한다.
- [x] `targetType`, `actorType`을 문자열 enum 대신 타입 안정적인 값으로 다루는 계층을 추가한다.

### 관리자/운영 도구

- [x] `Permission` row를 조회/추가/수정/삭제하는 admin UI를 만든다.
- [x] 특정 page/user 조합에 대해 어떤 row가 매칭되는지 보여주는 진단 화면을 만든다.

### 테스트

- [x] `WikiPermission` DB-only 동작 테스트를 추가한다.
- [x] 매칭 row가 없으면 denied 되는 테스트를 추가한다.
- [x] `PermissionLogic`의 target/actor 우선순위 조합 테스트를 보강한다.

## 별도 문서로 분리할 일

- hard-coded 관리자 판정(`aha00a@gmail.com` 또는 `seq == 1`)을 role/permission 기반으로 이관할지 결정한다.
- signed read URL을 일반 permission 모델 안으로 넣을지, 예외 경로로 유지할지 결정한다.
- 권한 admin UI의 화면 설계와 운영 절차를 별도 문서로 정리한다.
