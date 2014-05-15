#! /bin/sh

APPNAME="Dhek"
VER=`grep '^version:' dhek.cabal | cut -d ':' -f 2 | sed -e 's/^[ ]*//'`

# Path to previously built binary
BINPATH=`dirname $0`/../dist/build/dhek/dhek
 
BUILDDIR=`dirname $0`/../dist
BINFILE=`basename "$BINPATH"`
UTILDIR=`dirname $0`

mkdir -p "$BUILDDIR/$APPNAME.app/Contents/MacOS" 
mkdir -p "$BUILDDIR/$APPNAME.app/Contents/Resources/lib"
echo 'APPLdhek' > "$BUILDDIR/$APPNAME.app/Contents/PkgInfo"
sed -e "s/@VER@/$VER/g" < "$UTILDIR/macosx-app/Info.plist" > "$BUILDDIR/$APPNAME.app/Contents/Info.plist"
cp "$UTILDIR/macosx-app/dhek.icns" "$BUILDDIR/$APPNAME.app/Contents/Resources"
 
cp "$BINPATH" "$BUILDDIR/$APPNAME.app/Contents/MacOS/"
cp -R "$UTILDIR/../resources" "$BUILDDIR/$APPNAME.app/Contents/MacOS/resources"
 
echo "Will process dependencies ..."
DEPS=`otool -L "$BUILDDIR/$APPNAME.app/Contents/MacOS/$BINFILE" | grep '.dylib' | grep -v '@' | grep -v '/usr/lib' | awk '{ printf("%s\n", $1); }'`
 
for D in $DEPS; do
  N=`basename "$D"`
  echo "* $N"

  cp "$D" "$BUILDDIR/$APPNAME.app/Contents/Resources/lib"
  install_name_tool -change "$D" "@executable_path/../Resources/lib/$N" "$BUILDDIR/$APPNAME.app/Contents/MacOS/$BINFILE" 
done
