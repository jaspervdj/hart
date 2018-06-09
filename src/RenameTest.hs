import           Hart.ParseModule
import           Hart.Renamer
import qualified Language.Haskell.Exts as H
import           System.Environment

main :: IO ()
main = do
    [filePath] <- getArgs
    string <- readFile filePath
    case parseModule (Just filePath) string of
        Left err -> fail err
        Right m  ->
            let (newModule, _) = shitRenamer (fmap H.srcInfoSpan m) in
            putStrLn $ H.prettyPrint newModule
