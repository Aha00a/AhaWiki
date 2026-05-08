# ToDo: `AccessLog.url` 필드 제거

## 목표
`AccessLog` 테이블의 `url` 필드는 `scheme`, `host`, `uri`로 대체 가능한 중복 데이터이므로 제거합니다.

---

## 마이그레이션 계획
* [x] AccessLog.url 사용처 제거
* [x] NOT NULL -> NULL
* [ ] Admin에 Scheme Host URI를 조합하여 링크 생성 
* [ ] 추후 - 실제 필드제거
