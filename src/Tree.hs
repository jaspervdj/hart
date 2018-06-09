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

type IntervalForest a = [IntervalTree a]

data IntervalTree a
  = Empty
  | IntervalNode
      { start :: Int
      , end :: Int
      , payload :: a
      , iChildren :: IntervalForest a
      }
  deriving (Show)

instance (Show a) => Monoid (IntervalTree a) where
  mempty = Empty
  t `mappend` Empty = t
  Empty `mappend` t = t
  t1 `mappend` t2 = case graft t1 t2 of
    Just t3 -> t3
    Nothing -> error $ "bad: " ++ show t1 ++ ", " ++ show t2


data Tokn = Tokn
  { prefix :: C8.ByteString
  , content :: C8.ByteString
  , suffix :: C8.ByteString
  }
  deriving (Show)

pureTokn x = Tokn mempty x mempty

showTokn (Tokn a b c) = C8.concat [a, b, c]

data Tree
    = Node
      { context :: [Tokn]
      , children :: [Tree]
      }
    | NameLeaf Int
  deriving (Show)

numTokns :: Tree -> Int
numTokns (NameLeaf _) = 0
numTokns (Node toks children) = length toks + sum (map numTokns children)

data Program = Program Tree (VB.Vector C8.ByteString)

toTree :: C8.ByteString -> IntervalTree a -> Tree
toTree bs Empty = Node [pureTokn $ C8.pack ""] []
toTree bs (IntervalNode start' end' _ xs') = case subtrees of
    [] -> Node [subbs] []
    (x:_) -> Node (zipWith getSlice (start' : map end xs) (map start xs ++ [end'])) subtrees
  where
    xs = sortBy (comparing start) xs'
    subtrees = map (toTree bs) xs
    getSlice s e = pureTokn $ fst $ C8.splitAt (e - s) $ snd $ C8.splitAt (s - 1) bs
    subbs = getSlice start' end'

graft :: forall a. IntervalTree a -> IntervalTree a -> Maybe (IntervalTree a)
graft Empty t = Just t
graft t Empty = Just t
graft (IntervalNode s e a xs) t@(IntervalNode s' e' _ _)
  | s' >= s && e' <= e = Just $ case go xs of
      Nothing -> IntervalNode s e a (xs ++ [t])
      Just xs' -> IntervalNode s e a xs'
  | otherwise = Nothing
      where
        go [] = Nothing
        go (x:xs) = case graft x t of
          Just x' -> Just $ x' : xs
          Nothing -> (x:) <$> go xs

pp :: (Pretty a) => a -> String
pp = prettyPrintStyleMode style defaultMode {layout = PPNoLayout}

foo :: (Foldable f) => String -> f SrcSpanInfo -> IntervalForest [String]
foo bs = undefined

fName = "Main2.hs"

printProgram :: Program -> C8.ByteString
printProgram (Program (Node (label:[]) []) _) = showTokn label
printProgram (Program (Node (label:labels) (child:children)) names) =
    C8.concat [showTokn label, printProgram (Program child names), printProgram (Program (Node labels children) names)]
printProgram (Program (NameLeaf i) names) = names VB.! i
