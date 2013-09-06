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
function monospace($pdf, $x, $y, $height, $width, $count, $txt) {
  $pdf->SetFont("Courier", "", $fontSize); // monospace font

  //$cw = $width / $count;
  $cw = 12.85;

  $xi = $x + 1;
  $i = 0;
  foreach (str_split($txt) as $c) {
    $pdf->SetXY($xi, $y);
    $pdf->Cell($cw, $height, $c, true, "C");

    $xi += $cw + 1;

    if (++$i == $count) {
      break;
    }
  }
}

$json = json_decode(file_get_contents("test.json"));

require("fpdf/fpdf/fpdf.php");
require("fpdf/fpdi/fpdi.php");

$pdfSrcPath = "test.pdf";

$pdf = new FPDI("P", //L=>Landscape / P=>Portrait
		"pt" /* point */);

$fontSize = 14;

$pagecount = $pdf->setSourceFile($pdfSrcPath);
$testText = "abcdefghijklmnopqrstuvwxyz0123456789";

for ($i = 0; $i < $pagecount; $i++) {
  $pdf->AddPage();
  $tplIdx = $pdf->importPage($i+1);
  $pdf->useTemplate($tplIdx, 0, 0, 0 ,0, true);

  if (isset($json->pages[$i]) && isset($json->pages[$i]->areas)) {
    for ($j = 0; $j < count($json->pages[$i]->areas); $j++) {
      $area = $json->pages[$i]->areas[$j];
      $x = $area->x;
      $y = $area->y;
      $w = $area->width;
      $h = $area->height;

      // Draw blue rect at bounds
      $pdf->SetDrawColor(0,0,255);
      $pdf->SetLineWidth(0.2835);	
      $pdf->Rect($x, $y, $w, $h);

      if ($area->type == "checkbox") {
        $pdf->SetDrawColor(0,255,0);
        $pdf->SetLineWidth(2.0);
        $pdf->Line($x, $y, $x+$w, $y+$h);
        $pdf->Line($x, $y+$h, $x+$w, $y);
      } else if ($area->type == "monospaceText") {
        $pdf->SetDrawColor(0,255,0);
        monospace($pdf, $x, $y, 
          $h, $w, $area->cellCount,
          strtoupper($testText));

      } else {
        // 'Free' text
        $pdf->SetXY($x, $y);
        $pdf->MultiCell($w, intval($h), $area->name, true);
      }
    }
  } 
}

$pdf->Output("test-dhek.pdf", "F");
?>
```