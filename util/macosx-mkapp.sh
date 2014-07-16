#! /bin/sh

relocate() {
    OPATH="$1"
    LPATH="$2"
    RPATH="$3"

    ON=`basename "$OPATH"`
    DEPS=`otool -L "$OPATH" | grep -v "$ON" | grep '.dylib' | grep -v '@' | grep -v '/usr/lib' | sort -u | awk '{ printf("%s\n", $1); }'`

    THEN=""

    for D in $DEPS; do
        N=`basename "$D"`

        install_name_tool -change "$D" "$RPATH/$N" "$OPATH" 

        echo "* $N (from $ON)"

        if [ ! -f "$LPATH/$N" ]; then
            cp "$D" "$LPATH"
            chmod u+w "$LPATH/$N"
            install_name_tool -id "$RPATH/$N" "$LPATH/$N" 

            THEN="$THEN $LPATH/$N"
        fi
    done

    for D in $THEN; do
        relocate "$D" "$LPATH" "$RPATH"
    done
}

APPNAME="Dhek"
VER=`grep '^version:' dhek.cabal | cut -d ':' -f 2 | sed -e 's/^[ ]*//'`

# Path to previously built binary
if [ "$BINPATH" = "" ]; then
    BINPATH=`dirname $0`/../dist/build/dhek/dhek
fi

BUILDDIR=`dirname $0`/../dist
BINFILE=`basename "$BINPATH"`
UTILDIR=`dirname $0`

mkdir -p "$BUILDDIR/$APPNAME.app/Contents/MacOS" 
mkdir -p "$BUILDDIR/$APPNAME.app/Contents/Resources/lib/gtk-2.0/2.10.0"

echo 'APPLdhek' > "$BUILDDIR/$APPNAME.app/Contents/PkgInfo"
sed -e "s/@VER@/$VER/g" < "$UTILDIR/macosx-app/Info.plist" > "$BUILDDIR/$APPNAME.app/Contents/Info.plist"
cp "$UTILDIR/macosx-app/dhek.icns" "$BUILDDIR/$APPNAME.app/Contents/Resources"
 
cp "$BINPATH" "$BUILDDIR/$APPNAME.app/Contents/MacOS/"
cp -R "$UTILDIR/../resources" "$BUILDDIR/$APPNAME.app/Contents/MacOS/resources"
 
echo "Will process dependencies ..."
 
relocate "$BUILDDIR/$APPNAME.app/Contents/MacOS/$BINFILE" \
  "$BUILDDIR/$APPNAME.app/Contents/Resources/lib" \
  "@executable_path/../Resources/lib"

# GTK RC
echo "Will process GTK embedded setup ..."
GTK_ENGINE="$BUILDDIR/$APPNAME.app/Contents/Resources/lib/gtk-2.0/2.10.0/engines"

rm -rf "$GTK_ENGINE"
for L in `pkg-config --libs-only-L gtk+-2.0`; do
  D=`echo "$L" | sed -e 's/^-L//'`
  TD="$D/gtk-2.0/2.10.0/engines"

  if [ -d "$TD" ]; then
      echo "Copy GTK engine from $TD"
      cp -R "$TD" "$GTK_ENGINE"
  fi
done

cp -R "$UTILDIR/macosx-app/gtk2/themes" "$BUILDDIR/$APPNAME.app/Contents/Resources/themes"

for F in `ls -v -1 "$GTK_ENGINE/"`; do
    relocate "$GTK_ENGINE/$F" "$BUILDDIR/$APPNAME.app/Contents/Resources/lib" \
        "@executable_path/../Resources/lib"
done

# Pango
echo "Will process Pango embedded setup ..."

PANGO_VER="1.8.0"
PANGODIR="$BUILDDIR/$APPNAME.app/Contents/Resources/lib/pango/$PANGO_VER"
mkdir -p "$PANGODIR/modules"

PANGO_MODPATH=`pango-querymodules --system | grep 'ModulesPath' | sed -e 's|.*=[ \t]*/|/|;s|[ \t]*$||'`
PANGO_MODS=`ls -v -1 $PANGO_MODPATH/*.so`

for M in $PANGO_MODS; do
    N=`basename "$M"`
    cp "$M" "$PANGODIR/modules"

    chmod u+w "$PANGODIR/modules/$N"
    relocate "$PANGODIR/modules/$N" \
        "$BUILDDIR/$APPNAME.app/Contents/Resources/lib" \
        "@executable_path/../Resources/lib"

done

pango-querymodules --system | grep -v '^#' | sed -e "s|$PANGO_MODPATH/||" > \
    "$PANGODIR/pango.modules"

#cat > "$PANGODIR/pangorc" <<EOF
#[Pango]
# Relative to executable file
#ModuleFiles=../Resources/lib/pango/$PANGO_VER/pango.modules
#EOF
