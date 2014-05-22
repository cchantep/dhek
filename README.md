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

### Windows 7/8

TODO

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