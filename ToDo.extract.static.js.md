# ToDo: Extract static JS from `*.scala.html`

`app/views/**/*.scala.html` 전수 조사 결과, **정적(inline) JS를 외부 파일로 분리 가능한 대상은 8개 파일 / 12개 script block** 입니다.

기준:
- `<script src="...">` 는 제외
- `<script type="text/html">` 템플릿은 제외
- script 본문에 Twirl 변수/표현식(`@...`)이 없으면 정적으로 분류

## 분리 대상 디렉터리

- 기본 분리 위치: `public/js/`
- 뷰 경로를 반영한 권장 구조:
  - `app/views/Wiki/*.scala.html` → `public/js/Wiki/*.js`
  - `app/views/Admin/*.scala.html` → `public/js/Admin/*.js`
  - `app/views/Search/*.scala.html` → `public/js/Search/*.js`
  - `app/views/Test/*.scala.html` → `public/js/Test/*.js`
  - `app/views/macros/*.scala.html` → `public/js/macros/*.js`
  - 공통 베이스 템플릿(`_base*`) → `public/js/common/*.js`

## 대상 파일 목록

- [ ] `app/views/_baseSkeleton.scala.html` (정적 inline script 4개)
- [ ] `app/views/Wiki/graph3d.scala.html` (정적 inline script 1개)
- [ ] `app/views/Wiki/map.scala.html` (정적 inline script 1개)
- [ ] `app/views/Wiki/action.scala.html` (정적 inline script 1개)
- [ ] `app/views/Wiki/pageList.scala.html` (정적 inline script 1개)
- [ ] `app/views/Wiki/edit.scala.html` (정적 inline script 2개, 우선순위 높음)
- [ ] `app/views/Wiki/diff.scala.html` (정적 inline script 1개)
- [ ] `app/views/Test/gradient.scala.html` (정적 inline script 1개)

## 참고

- 위 목록에는 `Test` 뷰도 포함되어 있어, 운영 화면 기준으로는 우선순위를 조정할 수 있습니다.
- `@`가 포함된 dynamic script는 현재 기준으로는 분리 시 템플릿 변수 주입 방식(예: `data-*`, `window.__BOOTSTRAP__`) 설계가 필요합니다.
- 현재 단계는 문서 정리만 수행하며, 실제 JS 추출/치환 작업은 다음 스텝에서 진행합니다.
