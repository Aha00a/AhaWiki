#!/bin/sh

export JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64
export PATH="$JAVA_HOME/bin:$PATH"

nice -n 19 ~/sbt/bin/sbt stage

