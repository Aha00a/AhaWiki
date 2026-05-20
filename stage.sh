#!/bin/sh

export JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64
export PATH="$JAVA_HOME/bin:$PATH"

nice -n 19 "$JAVA_HOME/bin/java" -jar ~/sbt/bin/sbt-launch.jar stage

