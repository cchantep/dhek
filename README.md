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
cabal install gtk2hs-buildtools
cabal install gtk
```

#### Other (from sources or package manager)

```
cabal update
cabal install alex
cabal install happy
cabal install gtk2hs-buildtools
cabal install gtk
```

--

In order to build Dhek:
```
cabal install --only-dependencies
cabal configure
cabal build
```

You can produce an executable by doing:

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
