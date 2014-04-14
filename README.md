# Dhek

PDF app (or on the Discworld, supreme Djelibeybian god).

## Build

### Unix
Depending on how you've installed GHC, you have 2 different prerequisites.

Make sure gtk, pango, cairo and poppler are installed on your machine.

#### MacPorts

```
port install pango poppler cairo gtk2
```

#### Haskell Platform

```
cabal update
cabal install alex happy gtk2hs-buildtools gtk
```

#### Dhek sources

In order to build Dhek:

```
cabal install --only-dependencies
cabal configure
cabal build
```

In case of linking error about `_iconv`, re-run `cabal configure` with `--extra-lib-dir=/usr/lib` to enforce system version of libiconv is used.

You can install an executable by doing:

```
cabal install
```

At this point, Dhek can be launched with `./dist/build/dhek/dhek`

### Windows (only tested on Seven)

Make sure you've installed GTK all-in-one [32Bits](http://ftp.gnome.org/pub/gnome/binaries/win32/gtk+/2.24/gtk+-bundle_2.24.10-20120208_win32.zip) or [64Bits](http://ftp.gnome.org/pub/gnome/binaries/win64/gtk+/2.22/gtk+-bundle_2.22.1-20101229_win64.zip) and MinGW poppler and poppler-glib.

Note: Unfortunately, it only works when packages are installed globally.

```
cabal update
cabal install gtk2hs-buildtools
cabal install gtk
```

Then, in project directory:

```
cabal install --only-dependencies --global
runghc Setup configure --global # Don't ask me why it only works that way on Windows
runghc Setup build
```

To produce an exe:

```
runghc Setup install --global
```

At this point, Dhek can be launched with `HASKELL_HOME\bin\dhek.exe`

--

Anyway, for people working on crippled Operating Systems, we've already packaged Dhek for OSX and Windows (>= Seven) in [release section](https://github.com/applicius/dhek/releases)

### JSON format

Dhek mappings are saved in JSON files. Struture is the following:

```json
{
  "pages": [
    { /* mappings for first page, index 0 */
      "areas": [
        { /* first area of first page */
          "height": 10.23/* pt */,
          "width": 23.456,
          "x": 0.1234/* pt */,
          "y": 2.45,
          "name": "Field name",
          "type": "text"/* or "checkbox" */
        }
        /* , { ... }, ... */
      ]
    },
    null /* no mapping for second page, index 1 */
    /* , { third page, index 2 ... } */
  ]
}
```

These mappings can be used to merge dynamic data with original PDF, in order to generate a new PDF document.

Dhek is using UTF-8 character set, so so there is not issue to use space, ponctuation signs or accentuated character while preparing template (e.g. in area name).
