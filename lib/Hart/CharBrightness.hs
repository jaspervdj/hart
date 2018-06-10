{-# LANGUAGE BangPatterns #-}
module Hart.CharBrightness
    ( charToBrightness
    , brightnessToChar
    ) where

import           Data.Char           (chr, ord, isAlphaNum)
import           Data.List           (minimumBy)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           Data.Ord            (comparing)
import qualified Data.Vector.Unboxed as VU

charToBrightness :: Char -> Float
charToBrightness c
    | x < 0 || x >= 256 = error $ "Unknown character brightness: " ++ show c
    | otherwise         = vecCorrectedBrightness `VU.unsafeIndex` x
  where
    !x = ord c

brightnessToChar :: Float -> Char
brightnessToChar f =
    let idx = round ((1 - f) * 255) in
    if idx < 0 || idx > 255
        then error $ "float out of bounds: " ++ show f
        else vecBrightnessToChar VU.! idx

-- | Reversed mapping.
vecBrightnessToChar :: VU.Vector Char
vecBrightnessToChar = VU.generate 256 $ \idx ->
    let brightness = fromIntegral idx / 255 :: Float in
    minimumBy (comparing $ distance brightness) $ M.keys correctedBrightness
  where
    distance :: Float -> Char -> Float
    distance brightness c = abs (brightness - charToBrightness c)

-- | For faster access
vecCorrectedBrightness :: VU.Vector Float
vecCorrectedBrightness = VU.generate 256 $ \idx ->
    fromMaybe 0.0 $ M.lookup (chr idx) correctedBrightness

-- | 'sourceBrightness' scaled to have values in between [0, 1].
correctedBrightness :: M.Map Char Float
correctedBrightness = fmap
    (\x -> (x - low) / (high - low))
    sourceBrightness
  where
    low  = minimum $ map snd $ M.toList sourceBrightness
    high = maximum $ map snd $ M.toList sourceBrightness

-- | Stolen from
-- <https://dboikliev.wordpress.com/2013/04/20/image-to-ascii-conversion/>
sourceBrightness :: M.Map Char Float
sourceBrightness = M.fromList
    [ (' ', 0.00)
    , ('`', 1.20)
    , ('-', 1.60)
    , ('.', 1.60)
    , ('\'', 1.92)
    , ('_', 2.56)
    , (':', 3.20)
    , (',', 3.68)
    , ('"', 4.00)
    , ('=', 4.16)
    , ('^', 4.16)
    , (';', 5.12)
    , ('<', 5.28)
    , ('+', 5.44)
    , ('!', 5.52)
    , ('*', 5.68)
    , ('?', 6.40)
    , ('/', 6.48)
    , ('c', 6.48)
    , ('L', 6.56)
    , ('\\', 6.56)
    , ('z', 6.56)
    , ('r', 6.64)
    , ('s', 6.96)
    , ('7', 7.04)
    , ('T', 7.04)
    , ('i', 7.20)
    , ('v', 7.36)
    , ('J', 7.44)
    , ('t', 7.52)
    , ('C', 7.60)
    , ('{', 7.68)
    , ('3', 7.84)
    , ('F', 7.84)
    , (')', 8.00)
    , ('I', 8.00)
    , ('l', 8.00)
    , ('(', 8.08)
    , ('x', 8.08)
    , ('Z', 8.32)
    , ('f', 8.32)
    , ('Y', 8.40)
    , ('5', 8.48)
    , ('S', 8.48)
    , ('2', 8.64)
    , ('e', 8.64)
    , ('a', 8.88)
    , ('j', 8.88)
    , ('o', 8.88)
    , ('1', 8.96)
    , ('4', 8.96)
    , ('[', 8.96)
    , ('n', 8.96)
    , ('u', 8.96)
    , ('y', 9.04)
    , ('E', 9.12)
    , (']', 9.28)
    , ('P', 9.76)
    , ('6', 10.00)
    , ('V', 10.00)
    , ('9', 10.24)
    , ('k', 10.32)
    , ('X', 10.72)
    , ('p', 10.80)
    , ('K', 10.88)
    , ('w', 10.88)
    , ('G', 10.96)
    , ('h', 10.96)
    , ('q', 10.96)
    , ('A', 11.12)
    , ('U', 11.12)
    , ('b', 11.12)
    , ('O', 11.20)
    , ('d', 11.20)
    , ('8', 11.76)
    , ('#', 11.84)
    , ('H', 11.84)
    , ('R', 11.84)
    , ('D', 12.08)
    , ('B', 12.24)
    , ('0', 12.64)
    , ('$', 12.80)
    , ('m', 12.88)
    , ('g', 12.96)
    , ('M', 13.20)
    , ('W', 13.60)
    , ('&', 13.84)
    , ('Q', 13.84)
    , ('%', 14.40)
    , ('N', 14.48)
    , ('@', 19.12)
    ]
