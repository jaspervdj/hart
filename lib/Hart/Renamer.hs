module Hart.Renamer
    ( shitRenamer
    ) where

import qualified Data.Data.Extended                   as Data
import qualified Data.Map                             as M
import qualified Data.Set                             as S
import qualified Language.Haskell.Exts                as HE
import qualified Language.Haskell.Names               as HN
import qualified Language.Haskell.Names.ModuleSymbols as HN

shitRenamer :: HE.Module HE.SrcSpan -> (HE.Module HE.SrcSpan, [String])
shitRenamer module0 =
    let annotated0 = HN.annotate M.empty module0

        definitions = concat $ map definitionSites $ Data.grecQ annotated0

        locations =
            [ loc
            | (name, loc) <- definitions
            , not (name `S.member` toplevels module0)
            ]

        newnames = M.fromList $
            [ (loc, "shit" ++ show n)
            | (loc, n) <- zip locations [1 :: Int ..]
            ]

        unscope (HN.Scoped _ x) = x

        annotated1 = Data.grecT (plug newnames) annotated0 in

    (fmap unscope annotated1, map snd (M.toList newnames))

plug
    :: M.Map HE.SrcLoc String
    -> HE.Name (HN.Scoped HE.SrcSpan)
    -> HE.Name (HN.Scoped HE.SrcSpan)
plug newnames old = case old of
    HE.Ident (HN.Scoped HN.ValueBinder src) _ ->
        case M.lookup (HE.getPointLoc src) newnames of
            Nothing -> old
            Just n  -> HE.Ident (HN.Scoped HN.ValueBinder src) n
    HE.Ident (HN.Scoped (HN.LocalValue src) a) _ -> case M.lookup src newnames of
        Nothing -> old
        Just n  -> HE.Ident (HN.Scoped (HN.LocalValue src) a) n
    _ -> old

toplevels :: HE.Module HE.SrcSpan -> S.Set String
toplevels modul = S.fromList $
    [ str
    | HE.Ident _ str <- map HN.symbolName (HN.moduleSymbols M.empty modul)
    ]

definitionSites :: HE.Name (HN.Scoped HE.SrcSpan) -> [(String, HE.SrcLoc)]
definitionSites (HE.Ident (HN.Scoped HN.ValueBinder src) str) =
    [(str, HE.getPointLoc src)]
definitionSites _ = []
