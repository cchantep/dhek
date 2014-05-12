#! /bin/sh

OSVER=$1

VER=`grep '^version:' dhek.cabal | cut -d ':' -f 2 | sed -e 's/^[ ]*//'`

DISTDIR=dist/dhek-win$OSVER-$VER
mkdir "$DISTDIR"

cp dist/build/dhek/dhek "$DISTDIR"
cd "$DISTDIR"
ldd dhek.exe | grep '/usr/' | sed -e 's/^.*=>[ \t]*/cp /;s/\.dll.*$/.dll ./' | sh
