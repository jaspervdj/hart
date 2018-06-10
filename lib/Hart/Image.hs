module Hart.Image
    ( Config (..)
    , Image (..)
    , loadImage
    ) where

import qualified Codec.Picture       as Pic
import qualified Codec.Picture.Types as Pic
import           Control.Exception   (Exception, throwIO)
import qualified Data.Vector.Unboxed as VU
import           Data.Word           (Word8)

data LoadImageException = LoadImageException String

instance Show LoadImageException where show (LoadImageException msg) = msg

instance Exception LoadImageException

data Config = Config
    -- Number of columns before wrapping
    { cColumns   :: !Int
    -- Character width / character height
    , cFontRatio :: !Double
    }

data Image = Image
    { iRows    :: !Int
    , iColumns :: !Int
    , iData    :: !(VU.Vector Float)
    }

loadImage
    :: Config
    -> FilePath
    -> IO Image
loadImage config filePath = do
    errOrImg <- Pic.readImage filePath
    img <- either (throwIO . LoadImageException) return errOrImg
    case img of
        Pic.ImageRGB8 x  -> ok x
        Pic.ImageRGBA8 x -> ok $ Pic.pixelMap Pic.dropTransparency x
        Pic.ImageYCbCr8 x -> ok $ Pic.pixelMap Pic.convertPixel x
        _ -> throwIO $ LoadImageException "Unsupported image format"
  where
    ok = return . convert config

convert
    :: Config
    -> Pic.Image Pic.PixelRGB8
    -> Image
convert config image = Image
    { iRows = rows
    , iColumns = columns
    , iData = VU.generate (rows * columns) $ \idx ->
        let (row, col) = idx `divMod` columns
            startX = round (fromIntegral col * cellWidth)
            endX = boundw $ round (fromIntegral (col + 1) * cellWidth)
            startY = round (fromIntegral row * cellHeight)
            endY = boundh $ round (fromIntegral (row + 1) * cellHeight) in
        areaBrightness (startX, endX) (startY, endY) image
    }
  where
    width  = Pic.imageWidth  image
    height = Pic.imageHeight image
    boundw = min (width - 1)
    boundh = min (height - 1)

    -- We might cut a little bit from the bottom of the image by doing floor
    columns = cColumns config
    rows = floor (fromIntegral height / cellHeight) :: Int

    cellWidth  = fromIntegral width / fromIntegral columns :: Double
    cellHeight = cellWidth / cFontRatio config

areaBrightness
    :: (Int, Int)
    -> (Int, Int)
    -> Pic.Image Pic.PixelRGB8
    -> Float
areaBrightness (startX, endX) (startY, endY) image
    | size <= 0 = 0.5
    | otherwise = sum
        [ let Pic.PixelRGB8 r g b = Pic.pixelAt image x y in
          w2f r * 0.299 + w2f g * 0.587 + w2f b * 0.114
        | x <- [startX .. endX]
        , y <- [startY .. endY]
        ] / fromIntegral size
  where
    w2f :: Word8 -> Float
    w2f x = fromIntegral x / 255
    size = (endX - startX + 1) * (endY - startY + 1)
