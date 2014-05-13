#! /bin/sh

OSVER=$1

VER=`grep '^version:' dhek.cabal | cut -d ':' -f 2 | sed -e 's/^[ ]*//'`

DISTDIR=dist/dhek-win$OSVER-$VER
mkdir -p "$DISTDIR/bin"

cp dist/build/dhek/dhek.exe "$DISTDIR/bin/"
cp util/win-dist/dlls/*.dll "$DISTDIR/bin/"
cp -R util/win-dist/gtk-theme/* "$DISTDIR/"
cp -R resources "$DISTDIR/bin/resources"

cat > "$DISTDIR/dhek.bat" << EOF
start bin\dhek.exe
EOF

cd dist && zip dhek-win$OSVER-$VER.zip -r dhek-win$OSVER-$VER
