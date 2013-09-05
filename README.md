# Dhek

PDF app (or on the Discworld, supreme Djelibeybian god).

## Build

Prerequisites:
- GHC
- Cabal
- GTK
- Poppler (https://github.com/YoEight/poppler)

### Mac OS X

```
port ghc cabal-install
port cairo-devel pango
```

### Cabal

It's recommanded to ensure Cabal is up-to-date: `cabal update`

Programs alex and happy are required: `cabal install alex happy`

As Dhek UI is based on GTK, its bindings should be installed:
```
cabal install gtk2hs-buildtools
cabal install gtk
cabal install stm
```

Finally Dhek itself can be build:
```
cabal configure
cabal build
```

At this point, built Dhek can be launched with `./dist/build/dhek/dhek`

### JSON format

Dhek mappings are saved in JSON files. Struture is the following:

```javascript
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