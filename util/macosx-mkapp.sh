#! /bin/sh

APPNAME="Dhek"

# Path to previously built binary
BINPATH=`dirname $0`/../dist/build/dhek/dhek
 
BUILDDIR=`dirname $0`/../dist
BINFILE=`basename "$BINPATH"`
 
mkdir -p "$BUILDDIR/$APPNAME.app/Contents/MacOS/lib"
mkdir -p "$BUILDDIR/$APPNAME.app/Contents/Resources"
 
cp "$BINPATH" "$BUILDDIR/$APPNAME.app/Contents/MacOS/"
cp -R `dirname $0`/../resources "$BUILDDIR/$APPNAME.app/Contents/MacOS/resources"
 
echo "Will process dependencies ..."
DEPS=`otool -L "$BUILDDIR/$APPNAME.app/Contents/MacOS/$BINFILE" | grep '.dylib' | grep -v '@' | grep -v '/usr/lib' | awk '{ printf("%s\n", $1); }'`
 
for D in $DEPS; do
  N=`basename "$D"`
  echo "* $N"
 
  cp "$D" "$BUILDDIR/$APPNAME.app/Contents/MacOS/lib/"
  install_name_tool -change "$D" "@loader_path/lib/$N" "$BUILDDIR/$APPNAME.app/Contents/MacOS/$BINFILE" 
done
