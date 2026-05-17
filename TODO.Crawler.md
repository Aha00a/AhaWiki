# TODO: Crawler 

## 목표
- 비동기 처리 기반으로 안정적인 크롤링 수행.
- 크롤링 결과를 DB 테이블에 캐싱하여 성능 및 재활용성 확보.

## 범위
- 비동기 작업 처리(큐/워커) 방식 적용.
- 크롤링 결과 캐싱용 테이블 설계 및 적용.
- 캐시 TTL 6개월 적용.
- 캐시 관리 기능(조회/삭제/강제갱신 등) 제공.

## 요구사항 명확화 (문제 가능 지점 보완)
- [x] URL 정규화 - # 이후 제거
- [x] 캐시 키를 정규화 URL 기준으로 통일. 기대효과: 중복 캐시 감소, 동일 문서 캐시 hit rate 상승, URL 표현 차이로 인한 재크롤링 낭비 방지.

## 내재화 설계 TODO
- [ ] DB 스키마 설계
  - [x] 크롤링 결과 캐시 테이블
  - [x] `id` bigint autoincrement, `url`, `dateInserted`, `dateUpdated` 컬럼
  - [x] 인덱스(URL 해시, 만료시각, 상태)
- [ ] 캐시 TTL 180일 정책 적용. 180일 넘으면 스케쥴러로 삭제.
- [x] `stale-while-revalidate` - max-age 90일, swr 90일
- [ ] SSRF 방어(사설 IP, loopback, link-local, metadata endpoint 차단)
- [x] 크롤링 결과 캐시 테이블 생성(evolution 포함).
- [x] stale-while-revalidate 응답 처리
- [ ] 관리자 캐시 관리 기능 구현
  - [ ] URL별 캐시 조회
  - [ ] 캐시 삭제
  - [ ] 강제 재크롤링 트리거
- [ ] 운영 메트릭 추가(성공률, 평균 지연, 큐 적체, 도메인별 실패율).
