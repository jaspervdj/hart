module Main where

import qualified Data.Vector.Unboxed          as VU
import           Hart.CharBrightness
import qualified Hart.Image                   as Img
import qualified System.Console.Terminal.Size as TSize
import           System.Environment           (getArgs)

main :: IO ()
main = do
    [filePath] <- getArgs
    Just (TSize.Window _rows cols) <- TSize.size

    let config = Img.Config {Img.cColumns = cols, Img.cFontRatio = 0.4}
    image <- Img.loadImage config filePath
    putStrLn $ VU.toList $ VU.map brightnessToChar $ Img.iData image
