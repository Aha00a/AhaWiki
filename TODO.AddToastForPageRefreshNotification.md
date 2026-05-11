# TODO: Add Toast For Page Refresh Notification

## 목표
- [ ] 같은 페이지를 보고 있는 **다른 사용자**가 수정사항을 저장했을 때, 현재 페이지 사용자에게 "새로고침 필요" 토스트를 노출한다.

## 서버(WebSocket) 작업
- [x] 페이지 저장 성공 시점(`controllers.Wiki.save`)에 현재 페이지 room으로 `page.updated` 이벤트를 broadcast한다.
- [x] 이벤트 payload 스키마를 정의한다.
  - [x] `type`: `"page.updated"`
  - [x] `pageName`: 페이지 이름
  - [x] `revision`: 최신 revision 번호
  - [x] `editorNickname`: 저장 사용자 닉네임(가능 시)
  - [x] `dateInserted`: 서버 timestamp(ISO8601)
- [ ] 저장한 본인 연결(sender)에는 이벤트를 보내지 않도록 제외 로직을 확인/적용한다.
- [ ] 페이지 권한(읽기 권한) 범위 내에서만 이벤트가 전달되도록 기존 watch 권한 모델과 정합성을 확인한다.

## 클라이언트(view) 작업
- [x] `app/views/Wiki/view.scala.html`의 WebSocket `onmessage`에 `page.updated` 분기 처리를 추가한다.
- [x] 토스트 UI를 구현한다.
  - [x] 메시지: "This page has been updated. Would you like to refresh?"
  - [x] 버튼: `Refresh`
  - [x] `새로고침` 클릭 시 `window.location.reload()` 실행
- [x] 중복 알림 방지 로직을 추가한다.
  - [x] 동일 revision에 대해 토스트 1회만 표시
  - [x] 이미 토스트가 열려 있으면 내용 갱신 또는 무시 정책 확정

## UX/운영 고려사항
- [ ] 토스트 자동 닫힘 없음
- [ ] 저장 직후 사용자가 스크롤/입력 중일 때의 방해 최소화 정책을 정한다.
- [ ] 모바일/데스크톱에서 토스트 위치와 가독성을 점검한다.

## 테스트 체크리스트
- [ ] 브라우저 A/B에서 동일 페이지를 연다.
- [ ] A에서 편집 저장 시 B에서 토스트가 즉시 노출되는지 확인한다.
- [ ] B에서 `새로고침` 클릭 시 최신 revision으로 반영되는지 확인한다.
- [ ] A(저장 당사자)에는 토스트가 뜨지 않는지 확인한다.
- [ ] revision이 증가하지 않는 상황(예: 실패 저장)에서 알림이 발생하지 않는지 확인한다.
- [ ] 페이지 권한이 없는 사용자가 watch 연결/이벤트 수신을 못하는지 확인한다.
- [ ] WebSocket 재연결 후에도 동일하게 동작하는지 확인한다.

## 릴리즈 체크
- [ ] 변경사항을 문서화(간단 사용 시나리오 + 제한사항)한다.
- [ ] 운영 환경에서 WebSocket 에러/재연결 로그를 모니터링한다.
