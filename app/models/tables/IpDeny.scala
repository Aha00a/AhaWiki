package models.tables

import anorm._
import anorm.SqlParser.{flatten, long, str}
import com.aha00a.play.AnormSqlParser.localDateTime
import play.api.Logging

import java.sql.Connection
import java.time.LocalDateTime

case class IpDeny(
  seq: Long,
  ip: String,
  dateInserted: LocalDateTime,
)

object IpDeny extends Logging {
  def tupled: ((Long, String, LocalDateTime)) => IpDeny = (apply _).tupled

  def insert(ip: String, accessLog: Option[Long], reason: String)(implicit connection: Connection): Option[Long] = {
    SQL"""INSERT INTO IpDeny (ip, accessLog, reason) VALUES ($ip, $accessLog, $reason)""".executeInsert()
  }

  def selectCount(ip: String)(implicit connection: Connection): Long = {
    SQL"""SELECT COUNT(*) cnt FROM IpDeny WHERE ip = $ip""".as(long("cnt") single)
  }

  def selectLatest(ip: String)(implicit connection: Connection): Option[IpDeny] = {
    SQL"""SELECT seq, ip, dateInserted FROM IpDeny WHERE ip = $ip ORDER BY seq DESC LIMIT 1"""
      .as(long("seq") ~ str("ip") ~ localDateTime("dateInserted") singleOpt).map(flatten)
      .map(tupled)
  }

  // 보관 기간이 곧 차단 기간이다. selectLatest 는 행의 나이를 보지 않으므로, 행이 남아 있는
  // 동안 그 IP 는 계속 403 이다. 이전 값 -5 YEAR 는 사실상 영구 차단이었다.
  //
  // 실측(2026-08-03)으로 90일을 골랐다. 14,320개 IP 중 10,446개(73%)가 90일 이상 조용한데,
  // 최근 24시간 403 응답 746건 중 그런 IP 에서 온 것은 2건(0.3%)뿐이었다. 풀어줘도 다시
  // 긁으면 IpRateLimiter 가 30초 윈도로 즉시 재차단한다. 잃는 것이 거의 없다.
  //
  // 영구 차단을 그만둔 이유가 더 크다. 가정용 IP 는 재할당되므로 시간이 지나면 애먼 사용자를
  // 막는다. 실제로 서버 자신의 EIP 가 2026-06-01 에 등록돼 2026-08-03 까지 막혀 있었다.
  // UserViewHistory 의 3개월과도 맞다.
  //
  // limit 은 한 번에 지울 최대 행수다. 26,644행이 쌓여 있어 1000 이면 수렴에 하루가 걸린다.
  // 다른 두 테이블과 같은 10000 으로 맞춘다 (테이블 전체가 4.7MB 라 부담이 없다).
  def deleteExpired(limit: Int = 10000)(implicit connection: Connection): Int = {
    SQL"""
        DELETE FROM IpDeny
            WHERE seq < (
                SELECT MAX(seq)
                    FROM (
                    SELECT seq, dateInserted
                        FROM IpDeny
                        ORDER BY seq
                        LIMIT $limit
                ) T
                WHERE T.dateInserted < DATE_ADD(NOW(), INTERVAL -90 DAY)
            );
    """.executeUpdate()
  }
}
