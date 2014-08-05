{-# LANGUAGE QuasiQuotes #-}
--------------------------------------------------------------------------------
-- |
-- Module : Dhek.PDF.Inlined
--
--
--------------------------------------------------------------------------------
module Dhek.PDF.Inlined (pdfTemplate) where

--------------------------------------------------------------------------------
import Control.Monad (join)

--------------------------------------------------------------------------------
import Text.RawString.QQ

--------------------------------------------------------------------------------
import Dhek.PDF.Type

--------------------------------------------------------------------------------
both :: (a -> b) -> (a, a) -> (b, b)
both k (a, a') = (k a, k a')

--------------------------------------------------------------------------------
px :: Double
px = 3.779527559

--------------------------------------------------------------------------------
dimToPx :: PageDimension -> (Int, Int)
dimToPx pg
    = both (truncate . (*px)) $
          case pg of
              A2_P -> (420, 594)
              A2_L -> (594, 420)
              A3_P -> (420, 297)
              A3_L -> (297, 420)
              A4_P -> (210, 297)
              A4_L -> (297, 210)
              A5_P -> (210, 148)
              A5_L -> (148, 210)

--------------------------------------------------------------------------------
pdfTemplateDim :: PageDimension -> String
pdfTemplateDim pd
    = let (w,h) = dimToPx pd in
    "/MediaBox [ 0 0 " ++ show w ++ " " ++ show h ++ " ]\n"

--------------------------------------------------------------------------------
pdfTemplatePages :: PageDimension -> PageCount -> String
pdfTemplatePages pd (PageCount c)
    = let dim = pdfTemplateDim pd
          count = "/Count " ++ show c ++ "\n"
          kids  = "/Kids [" ++ (join $ replicate c "3 0 R\n") ++ "]\n" in
      "2 0 obj\n<<\n" ++ dim ++ count ++ kids ++ ">>\nendobj\n\n"

--------------------------------------------------------------------------------
pdfHeader :: String
pdfHeader
    = [r|%PDF-1.7

1 0 obj  % entry point
<<
  /Type /Catalog
  /Pages 2 0 R
>>
endobj|]

--------------------------------------------------------------------------------
pdfFooter :: String
pdfFooter = [r|3 0 obj
<<
  /Type /Page
  /Parent 2 0 R
>>
endobj

xref
0 6
0000000000 65535 f
0000000010 00000 n
0000000079 00000 n
0000000173 00000 n
0000000301 00000 n
0000000380 00000 n
trailer
<<
  /Size 6
  /Root 1 0 R
>>
startxref
492
%%EOF|]

--------------------------------------------------------------------------------
pdfTemplate :: PageDimension -> PageCount -> String
pdfTemplate pd pc
    = let pages = pdfTemplatePages pd pc in
    pdfHeader ++ pages ++ pdfFooter
