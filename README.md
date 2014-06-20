# Dhek

PDF templating application (or on the Discworld, supreme Djelibeybian god).

## Motivation

Starting from any PDF, Dhek allows to define areas you want to fill with data later (templating).

Unlike other solution, it doesn't require PDF to have been made using Acrofields, or any PDF extension/constraint. Moreover, you don't need to alter PDF in any way to be able to create a template. *It can be done above any existing PDF*.

[Template format](#template-format) is an open one (JSON), so that you can use it whatever is your programming/integration environment (PHP, JVM, .Net, ...).

You can also use a [service](http://go.applidok.com) allowing Web visitor to fill data for your templates using form, and finally get their custom/filled PDF.

## Get started

Binary distributions for Windows (7+) and Mac OS X (10.8/10.9) can be found in [releases section](https://github.com/applicius/dhek/releases).

## Build

You can also build Dhek by yourself with following information.

Depending on how you've installed GHC, you have 2 different prerequisites.

Make sure gtk, pango, cairo and poppler are installed on your machine.

### Haskell Platform

#### Linux

```
cabal update
cabal install alex happy gtk2hs-buildtools gtk
```

#### Windows 7/8

TODO

#### Mac OS X

**Prerequisites:**

- Fresh Haskell environment (no cabal installed in user space).
- Native development libraries: `port install pango poppler cairo gtk3`

**Cabal:**

Setup cabal-install 1.18:

```
# Upgrade transformer in user space
cd /tmp
curl -o - http://hackage.haskell.org/package/transformers-0.4.1.0/transformers-0.4.1.0.tar.gz | tar -xzvf -
cd transformers-0.4.1.0
runghc Setup.hs configure --user
runghc Setup.hs install

# Build cabal-install with updated dependencies (mtl, transformers, http)
git clone git@github.com:haskell/cabal.git --branch 1.18
cd cabal/cabal-install
sed -e 's/MTL_VER="2.1.2"/MTL_VER="2.2.1"/;s/TRANS_VER="0.3.0.0"/TRANS_VER="0.4.1.0";s/HTTP_VER="4000.2.11"/HTTP_VER="4000.2.17"/' < bootstrap.sh > .tmp && mv .tmp bootstrap.sh

# Setup cabal in user space
cabal update
cabal install alex happy gtk2hs-buildtools
```

> In case of linking error about `_iconv`, re-run `cabal configure` with `--extra-lib-dir=/usr/lib` to enforce system version of libiconv is used.

### Dhek sources

In order to build Dhek:

```
# Resolve dependencies
cabal install --only-dependencies

# Resolve poppler again, with required gtk3 flag
cabal install --reinstall poppler -f gtk3

cabal configure
cabal build
```

At this point, Dhek can be launched with `./dist/build/dhek/dhek`

### Template format

Dhek templates are saved in JSON files. Struture is the following:

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
