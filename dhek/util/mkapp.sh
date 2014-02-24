#! /bin/sh
 
BUILDDIR=`dirname $0`/dist
APPNAME="$1"
BINPATH="$2"
BINFILE=`basename "$BINPATH"`
 
mkdir -p "$BUILDDIR/$APPNAME.app/Contents/MacOS/lib"
mkdir -p "$BUILDDIR/$APPNAME.app/Contents/Resources"
 
cp "$BINPATH" "$BUILDDIR/$APPNAME.app/Contents/MacOS/"
 
echo "Will process dependencies ..."
DEPS=`otool -L "$BUILDDIR/$APPNAME.app/Contents/MacOS/$BINFILE" | grep '.dylib' | grep -v '@' | grep -v '/usr/lib' | awk '{ printf("%s\n", $1); }'`
 
for D in $DEPS; do
  N=`basename "$D"`
  echo "* $N"
 
  cp "$D" "$BUILDDIR/$APPNAME.app/Contents/MacOS/lib/"
  install_name_tool -change "$D" "@loader_path/lib/$N" "$BUILDDIR/$APPNAME.app/Contents/MacOS/$BINFILE" 
done