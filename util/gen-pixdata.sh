#! /bin/sh

BASEDIR=`dirname $0`
RESDIR="$BASEDIR/resources"

PIXDATA=""

for I in `find "$RESDIR" -type f -name '*.png' -print`; do
  N=`basename "$I" | sed -e 's/.png$//'`
  NN=`echo "$N" | sed -e 's/-/_/g'`
  CN=`echo "$N" | sed -e 's/-/_/g;s/$/.c/'`

  echo '#include <gdk/gdk.h>' > "$RESDIR/$CN"
  gdk-pixbuf-csource --raw --extern --name=$NN "$RESDIR/$N.png" >> "$RESDIR/$CN"
  PIXDATA="$PIXDATA resources/$CN"
done

echo "Generated:$PIXDATA"
