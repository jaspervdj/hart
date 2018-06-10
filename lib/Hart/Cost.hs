module Hart.Cost where

import Hart.Image
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import Data.Word

{-
costFunction :: Image -> Program -> Float
costFunction Image {..} prog =
  where
    printed = printProgram prog
    splitProg =

lineCost :: Image -> C8.ByteString -> Float
lineCost Image {..} bs = go 0 0 0 0
  where
    go oldCost imgRow imgCol bsIdx
        | imgIdx == VU.length iData = newCost
        | imgCol >= iColumns - 1 = go newCost (imgRow + 1) 0 
        | bsVal == '\n'
        | otherwise =
      where
        imgIdx = imgRow * iColumns + imgCol
        imgVal = iData VU.! imgIdx
        bsVal = C8.index bs bsIdx
        newCost = oldCost + sq (charToBrightness bsVal - imgVal)
        sq x = x * x
-}

foldByteStringAsImage
    :: (Int, Int)           -- ^ Columns, Rows
    -> B.ByteString
    -> (Int -> Int -> Word8 -> acc -> acc)
    -> acc
    -> acc
foldByteStringAsImage (imgCols, imgRows) bs f = C8.foldl' 
  where
    go idx x y
        | i
