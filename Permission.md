# Permission

이 문서는 현재 코드에 구성되어 있는 사용자 Permission 관련 구조를 정리한다.

## 요약

현재 실제 위키 페이지 접근 제어는 `Permission` 테이블을 우선 사용하고, 매칭되는 row가 없을 때만 페이지 본문 상단의 shebang directive와 세션 로그인 정보로 fallback한다.

- 읽기 권한: `#!read ...`
- 쓰기 권한: `#!write ...`
- 기본 읽기 권한: `.config`의 `permission.default.read`, 없으면 `all`
- 기본 쓰기 권한: `.config`의 `permission.default.write`, 없으면 `login`
- 로그인 사용자 식별: Play session의 `seq`, `email`, `nickname`
- 관리자 판정: `email == "aha00a@gmail.com"` 또는 `seq == 1`

`Permission` 테이블에 매칭 row를 추가하면 해당 row가 `#!read`/`#!write` directive보다 우선 적용된다. 명시적으로 거부하려면 `action = 0` row를 둔다.

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

세션에 `seq`, `email`, `nickname` 중 하나라도 없으면 비로그인 사용자로 취급된다.

## WikiPermission

실제 페이지 읽기/쓰기 권한 판정은 `logics.wikis.WikiPermission`이 담당한다.

판정 순서:

1. 현재 사이트의 `Permission` rows를 조회한다.
2. 요청 page name과 현재 actor email에 가장 우선순위가 높은 row를 찾는다.
3. 매칭 row가 있으면 `action >= requiredAction`으로 허용 여부를 결정한다.
4. 매칭 row가 없으면 legacy directive/default 방식으로 fallback한다.

### Directive 추출

`PageContent`는 페이지 본문 맨 앞에서 `#!`로 시작하는 줄들을 directive로 추출한다.

예:

```wiki
#!read all
#!write login

= Page Title
본문...
```

`PageContent`가 인식하는 권한 directive는 다음과 같다.

- `#!read <directive>`
- `#!write <directive>`

본문 첫 줄들이 `#!`로 시작하는 동안만 directive로 취급된다. 권한 directive가 없으면 사이트 설정 기본값을 사용한다.

### 기본값

기본값은 `AhaWikiConfig().permission.default`에서 읽는다. 이 값은 사이트별 `.config` 페이지의 HOCON 설정을 통해 공급된다.

- `permission.default.read`: 없으면 `all`
- `permission.default.write`: 없으면 `login`

따라서 별도 설정이 없는 페이지는 기본적으로 모두 읽을 수 있고, 로그인 사용자만 쓸 수 있다.

### Directive 문법

directive 값은 comma-separated list다.

```wiki
#!read all
#!write login
```

```wiki
#!read login
#!write aha00a@gmail.com,@example.com
```

각 항목의 의미는 다음과 같다.

- `all`: 비로그인 사용자를 포함한 모든 사용자 허용
- `login`: 로그인한 모든 사용자 허용
- `user@example.com`: 해당 이메일 사용자만 허용
- `@example.com`: 해당 도메인으로 끝나는 이메일 사용자 허용

매칭 로직은 다음과 같다.

- 로그인 사용자는 directive에 `all`, `login`, 본인 이메일, 또는 본인 이메일 suffix와 일치하는 `@domain` 항목이 있으면 허용된다.
- 비로그인 사용자는 directive에 `all`이 있을 때만 허용된다.

## 읽기 권한 적용 지점

페이지 조회에서 `Wiki.view`는 최신 revision의 본문으로 읽기 권한을 계산한다.

읽기가 허용되면 다음 action을 사용할 수 있다.

- `view`
- `raw`
- `history`
- `blame`
- `diff`

읽기가 거부되면 `Permission denied.`가 반환된다.

단, signed read URL이 유효하면 `#!read` 권한을 우회해 읽을 수 있다. signed read URL은 `sr_exp`, `sr_sig` query parameter와 `play.http.secret.key` 기반 HMAC으로 검증된다.

## 쓰기 권한 적용 지점

쓰기 권한은 최신 revision의 본문 기준으로 계산된다.

쓰기 권한이 있어야 가능한 동작:

- 없는 페이지의 `edit`
- 기존 페이지의 `edit`
- `rename`
- `delete`
- save
- attachment upload/delete 관련 일부 경로
- diary write
- Google Spreadsheet sync

저장 시에는 form body의 새 본문이 아니라 DB의 최신 본문(`latestText`)에 대한 `WikiPermission().isWritable(PageContent(latestText))`를 검사한다. 즉, 현재 페이지를 쓸 권한이 있어야 새 revision을 만들 수 있다.

새 revision 저장 시 `PageLogic.insert`는 새 본문에서 `PageContent(body).read`를 읽어 `Page.permRead`에 저장한다. 이후 `PageMeta` 계산 시에도 최신 revision의 `permRead`가 snapshot으로 들어간다.

## 목록/검색/매크로에서의 권한 필터링

페이지 목록 권한 필터링은 `PageLogic.getListPageByPermission`에서 수행된다.

흐름:

1. `PageMeta` 기반 최신 페이지 목록을 가져온다.
2. 각 row의 `permRead`를 읽는다.
3. `permRead`가 비어 있으면 `permission.default.read`를 사용한다.
4. 현재 사용자 이메일 기준으로 `WikiPermission.allowed`를 적용한다.

이 결과는 `ContextSite.seqPageByPermission`, `seqPageNameByPermission`, `setPageNameByPermission`으로 노출된다.

다음 기능들은 이 권한 필터링 결과를 사용한다.

- Home의 랜덤 페이지 선택
- Feed의 최근 페이지 목록
- `MacroPageList`
- `MacroTitleIndex`
- `MacroPageCount`
- calendar/navigation/map/schema 관련 일부 interpreter/macro
- API의 페이지 목록 계열 응답

검색 결과도 `Search.search`에서 각 결과 본문의 `PageContent`를 기준으로 `WikiPermission.isReadable`을 다시 적용한다.

## 관리자 권한

관리자 화면/API의 권한은 별도의 role 테이블이 아니라 하드코딩된 조건으로 판정한다.

```scala
SessionLogic.getUser(request).exists(u => u.email == "aha00a@gmail.com" || u.seq == 1)
```

이 조건은 `Admin`, `Api`, `ApiCrawler` 컨트롤러의 관리자 기능에서 사용된다.

관리자는 다음과 같은 기능에 접근할 수 있다.

- 사이트 목록/사이트 사용자 목록
- 사용자 목록/사용자 조회 이력
- 접근 로그
- 최근 변경 목록
- PageMeta 목록 및 재계산
- 사이트 favicon/theme 설정
- S3 object 관리
- crawler cache 관리
- memory cache 상태 조회

## UserSite

`UserSite` 테이블은 사용자와 사이트의 연결 정보를 저장한다.

스키마:

- `user`
- `site`
- `created`
- primary key: `(user, site)`
- `User`, `Site` foreign key

현재 코드 기준으로 `UserSite`는 주로 관리자 화면의 사이트별 사용자 목록과 통계성 조회에 사용된다. 일반 위키 페이지의 읽기/쓰기 권한 판정에는 직접 사용되지 않는다.

## Permission 테이블

`Permission` 테이블은 evolution 18에서 추가되어 있다.

스키마:

- `site`
- `target`
- `actor`
- `action`
- `created`
- primary key: `(site, target, actor)`

모델 상 action 값은 다음과 같다.

| 이름 | 값 |
| --- | ---: |
| `none` | 0 |
| `read` | 1 |
| `edit` | 2 |
| `create` | 4 |
| `upload` | 8 |
| `delete` | 16 |
| `admin` | 255 |

`Permission.permitted(action)`은 `this.action >= action`으로 판정한다. 즉 action 값은 bit flag가 아니라 등급처럼 동작한다.

### target 매칭

- `""`: 모든 target
- `"Private"`: 정확히 `Private`만
- `"Private?"`: `Private`로 시작하는 모든 target

`?` suffix는 prefix match 의미다.

### actor 매칭

- `""`: 모든 actor
- `"login"`: 로그인한 모든 actor
- `"@gmail.com"`: 이메일이 `@gmail.com`으로 끝나는 actor
- `"aha00a@gmail.com"`: 정확히 해당 이메일 actor

### 우선순위

`PermissionLogic`은 permission 목록을 `priority` 내림차순으로 정렬한 뒤, 처음 매칭되는 row를 사용한다.

priority는 `targetLevel + actorLevel`이다.

target level:

- 전체 target: 1
- prefix target(`?`): 2
- exact target: 3

actor level:

- 전체 actor: 1
- domain actor(`@...`): 2
- exact actor: 3

따라서 exact target + exact actor가 가장 우선된다.

## Permission 테이블 적용 상태

현재 주요 권한 판정 경로는 `PermissionLogic`을 사용한다. 단, 기존 데이터를 즉시 잠그지 않기 위해 매칭되는 `Permission` row가 없으면 legacy directive/default 정책으로 fallback한다.

현재 확인되는 사용처:

- `WikiPermission`: 실제 읽기/쓰기 권한 판정
- `PermissionUnit`, `PermissionLogicUnit`: 모델/로직 단위 테스트
- `Test.permission`: dev 환경에서 legacy directive 방식과 `PermissionLogic` 결과를 비교

따라서 실제 운영 정책은 `Permission` 테이블 row로 관리할 수 있다. row가 없는 page/actor 조합은 기존 `#!read`, `#!write`, `.config` 기본값을 따른다.

## PageMeta와 permRead

`Page.permRead`는 최신 revision의 읽기 directive snapshot이다.

저장 시:

- `PageLogic.insert`가 새 본문에서 `#!read` 값을 추출해 `Page.permRead`에 저장한다.

계산 시:

- `PageLogic.calculate`가 최신 page의 `permRead`를 `PageMeta.permRead`에 upsert한다.

목록 필터링 시:

- `PageMeta.permRead`를 사용해 페이지 목록을 사용자별로 필터링한다.

즉 전체 본문을 매번 읽지 않고도 목록/매크로/API에서 읽기 권한 필터링을 수행하기 위해 `PageMeta.permRead`가 사용된다.

## 예시

모두 읽기, 로그인 사용자만 쓰기:

```wiki
#!read all
#!write login
```

로그인 사용자만 읽기/쓰기:

```wiki
#!read login
#!write login
```

특정 사용자만 읽기/쓰기:

```wiki
#!read aha00a@gmail.com
#!write aha00a@gmail.com
```

특정 도메인 사용자에게 쓰기 허용:

```wiki
#!read all
#!write @example.com
```

여러 조건 조합:

```wiki
#!read login,@example.com
#!write aha00a@gmail.com,@example.com
```

비로그인 사용자는 `login`, 이메일, 도메인 조건을 만족할 수 없고 `all`만 만족한다.

Permission table로 같은 정책을 표현하는 예:

```sql
-- 모든 사용자 읽기 허용
INSERT INTO Permission (site, target, actor, action)
VALUES (1, '', '', 1);

-- 로그인 사용자 편집/생성 허용
INSERT INTO Permission (site, target, actor, action)
VALUES (1, '', 'login', 4);

-- 특정 prefix 페이지는 특정 사용자만 관리
INSERT INTO Permission (site, target, actor, action)
VALUES (1, 'Private?', 'aha00a@gmail.com', 255);

-- 특정 prefix 페이지를 기본적으로 차단
INSERT INTO Permission (site, target, actor, action)
VALUES (1, 'Private?', '', 0);
```

`PermissionLogic`은 더 구체적인 target/actor 조합을 우선 적용하므로, 위 예시에서 `Private? + aha00a@gmail.com` row가 `Private? + all` 차단 row보다 먼저 매칭된다.
