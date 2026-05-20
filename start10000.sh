#!/bin/sh

export JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64
export PATH="$JAVA_HOME/bin:$PATH"

exec target/universal/stage/bin/ahawiki -Dconfig.file=conf/wiki.aha00a.com.conf -Duser.timezone=Asia/Seoul -Dhttp.port=10000 -Dpidfile.path=RUNNING_PID_10000

