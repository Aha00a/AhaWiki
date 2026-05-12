# Page Refresh Notification

## 개요
같은 위키 페이지를 여러 사용자가 동시에 보고 있을 때, **다른 사용자가 저장한 변경사항**을 감지해 현재 사용자에게 새로고침 안내 토스트를 보여주는 기능입니다.

## 현재 구현 완료 사항

### 1) 서버 이벤트 발행
- 페이지 저장 성공 시점에 해당 페이지 room으로 `page.updated` 이벤트를 broadcast 합니다.
- 이벤트 payload는 아래 스키마를 사용합니다.
  - `type`: `"page.updated"`
  - `pageName`: 페이지 이름
  - `revision`: 최신 revision 번호
  - `editorNickname`: 저장 사용자 닉네임(가능 시)
  - `dateInserted`: 서버 timestamp(ISO8601)

### 2) 클라이언트 수신/처리
- `Wiki/view` 페이지의 WebSocket `onmessage`에서 `page.updated` 이벤트를 처리합니다.
- 이벤트 수신 시 토스트를 노출합니다.
  - 메시지: `This page has been updated. Would you like to refresh?`
  - 버튼: `Refresh`
  - 버튼 클릭 시 `window.location.reload()` 실행

### 3) 중복 알림 방지
- 동일 revision에 대해서는 토스트를 1회만 표시합니다.
- 이미 토스트가 떠 있는 상황에서의 중복 표시를 방지합니다.

## 현재 동작 시나리오
1. 사용자 A와 B가 같은 페이지를 열어둡니다.
2. A가 페이지를 저장합니다.
3. 서버가 `page.updated` 이벤트를 페이지 room에 전송합니다.
4. B의 화면에서 새로고침 안내 토스트가 표시됩니다.
5. B가 `Refresh`를 누르면 페이지가 새로고침되어 최신 revision을 확인할 수 있습니다.

## 참고 (남은 확인/운영 항목)
아래 항목은 별도 점검이 필요한 운영/정합성 이슈입니다.
- 저장 당사자(sender) 제외 전송 보장 확인
- watch 권한 모델과 이벤트 수신 범위 정합성 확인
- 토스트 UX(입력 중 방해 최소화, 모바일/데스크톱 가독성) 점검
- 운영 환경 WebSocket 에러/재연결 로그 모니터링
