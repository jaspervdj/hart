module Hart.Cost
    ( distance
    ) where

import qualified Data.Vector.Unboxed          as VU
import           Hart.CharBrightness
import           Hart.Image                   (Image(..))
import qualified Hart.Annealing               as Annealing
import qualified Data.ByteString.Char8        as C8

-- Assuming the bytestring contains all required newlines
distance :: Image -> C8.ByteString -> Annealing.E
distance image = VU.sum . VU.zipWith squareDiff (iData image) . VU.map charToBrightness . VU.fromList . C8.unpack
  where
    squareDiff :: Float -> Float -> Float
    squareDiff i c = (i - c) * (i - c)
