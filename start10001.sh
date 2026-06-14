#!/bin/sh

export JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64
export PATH="$JAVA_HOME/bin:$PATH"
JAVA_OPTS="${JAVA_OPTS:--J-Xms128m -J-Xmx512m -J-XX:+ExitOnOutOfMemoryError}"

exec target/universal/stage/bin/ahawiki $JAVA_OPTS -Dconfig.file=conf/wiki.aha00a.com.conf -Duser.timezone=Asia/Seoul -Dhttp.port=10001 -Dpidfile.path=RUNNING_PID_10001
