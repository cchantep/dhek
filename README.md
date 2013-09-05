# Dhek

PDF app (or on the Discworld, supreme Djelibeybian god).

## Build

Prerequisites:
- GHC
- Cabal
- GTK
- [Poppler](https://github.com/YoEight/poppler)

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

These mappings can be used to merge dynamic data with original PDF,
in order to generate a new PDF document.

#### PHP integration

Dhek mappings can be used in PHP using [FPDF](http://www.fpdf.org/) library:

```php
<?php
$json = json_decode(file_get_contents("test.json"));

require("fpdf/fpdf/fpdf.php");
require("fpdf/fpdi/fpdi.php");

// Must contain files DejaVuSansCondensed-Bold-ISO-8859-15.{z, php}
define("FPDF_FONTPATH", "fpdf/fpdf/font/");

$pdfSrcPath = "test.pdf";

$pdf = new FPDI("P", //L=>Landscape / P=>Portrait
		"pt" /* point */);

// Declares DejaVuSansCondensed-Bold.* files as DejaVuSansCondensed bold font
// This font use only
$pdf->AddFont("DejaVuSansCondensed","",
  "DejaVuSansCondensed-Bold-ISO-8859-15.php");

// Sets newly added font (as embedded)
$pdf->SetFont('DejaVuSansCondensed','','7');
$lineheight = 2.5;

$pagecount = $pdf->setSourceFile($pdfSrcPath);

for ($i = 0; $i < $pagecount; $i++) {
  $pdf->AddPage();
  $tplIdx = $pdf->importPage($i+1);
  $pdf->useTemplate($tplIdx, 0, 0, 0 ,0, true);

  if (isset($json->pages[$i]) && isset($json->pages[$i]->areas)) {
    for ($j = 0; $j < count($json->pages[$i]->areas); $j++) {
      $area = $json->pages[$i]->areas[$j];

      // Draw blue rect at bounds
      $pdf->SetDrawColor(0,0,255);
      $pdf->SetLineWidth(0.2835);	
      $pdf->Rect($area->x, $area->y, $area->width, $area->height);

      // Draw green labels
      $pdf->SetDrawColor(0,255,0);
      $pdf->SetXY($area->x, $area->y); // top-left padding: 1.0
      $pdf->MultiCell(50, $lineheight, $area->name);
    }
  } 
}

$pdf->Output("test-dhek.pdf", "F");
?>
```