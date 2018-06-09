module Hart.ParseModule
    ( parseModule
    ) where

import           Data.List             (nub)
import           Data.Maybe            (fromMaybe)
import qualified Language.Haskell.Exts as H

--------------------------------------------------------------------------------
-- | Syntax-related language extensions are always enabled for parsing. Since we
-- can't authoritatively know which extensions are enabled at compile-time, we
-- should try not to throw errors when parsing any GHC-accepted code.
defaultExtensions :: [H.Extension]
defaultExtensions = map H.EnableExtension
  [ H.GADTs
  , H.HereDocuments
  , H.KindSignatures
  , H.NewQualifiedOperators
  , H.PatternGuards
  , H.StandaloneDeriving
  , H.UnicodeSyntax
  ]

--------------------------------------------------------------------------------
-- | If the given string is prefixed with an UTF-8 Byte Order Mark, drop it
-- because haskell-src-exts can't handle it.
dropBom :: String -> String
dropBom ('\xfeff' : str) = str
dropBom str              = str

--------------------------------------------------------------------------------
-- | Abstraction over HSE's parsing
parseModule :: Maybe FilePath -> String -> Either String (H.Module H.SrcSpanInfo)
parseModule mfp string = do
    -- Determine the extensions: those specified in the file and the extra ones
    let processed        = dropBom string
        (lang, fileExts) = fromMaybe (Nothing, []) $ H.readExtensions processed
        exts             = nub $ fileExts ++ defaultExtensions

        -- Parsing options...
        fp       = fromMaybe "<unknown>" mfp
        mode     = H.defaultParseMode
            { H.extensions   = exts
            , H.fixities     = Nothing
            , H.baseLanguage = case lang of
                Nothing -> H.baseLanguage H.defaultParseMode
                Just l  -> l
            }

    case H.parseModuleWithComments mode processed of
        H.ParseOk (md, _) -> return md
        err               -> Left $
            "Language.Haskell.Stylish.Parse.parseModule: could not parse " ++
            fp ++ ": " ++ show err
