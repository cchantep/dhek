#! /bin/sh

DIR=`dirname $0`

javac -classpath "$DIR/jpedal_lgpl.jar" JPanelDemo.java && \
  java -classpath "$DIR/jpedal_lgpl.jar:." JPanelDemo
