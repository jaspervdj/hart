{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Tree where

import qualified Data.ByteString.Char8 as C8
import Data.Foldable
import Data.List
import Data.Ord
import qualified Data.Vector as VB
import Language.Haskell.Exts
import Debug.Trace


main :: IO ()
main = do
  content <- readFile fName
  case parseModule content of
    ParseOk mod@(Module _ _ _ _ _) -> do
      let onelined = pp mod

      case parseModule onelined of
        ParseOk mod2@(Module _ _ _ _ decls) -> do
          putStrLn onelined
          print mod2
          let iTree = foldl' (\t1 SrcSpanInfo {..} -> t1 `mappend` IntervalNode (srcSpanStartColumn srcInfoSpan) (srcSpanEndColumn srcInfoSpan) "" []) mempty mod2
          print iTree
          let ttree = toTree (C8.pack onelined) iTree
          print ttree
          C8.putStrLn $ printProgram $ Program ttree VB.empty
      -- print $ words onelined
    ParseFailed loc str -> traceShowM loc >> traceShowM str
{-# INLINE main #-}
