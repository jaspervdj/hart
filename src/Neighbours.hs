{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Neighbours where

import Control.Monad.Primitive
import qualified Data.ByteString.Char8 as C8
import qualified System.Random.MWC as MWC

import Tree


changeFromAlphabet :: forall m. PrimMonad m => C8.ByteString -> MWC.Gen (PrimState m) -> C8.ByteString -> m C8.ByteString
changeFromAlphabet alphabet gen ws = if C8.null ws
  then do
    r1 <- MWC.uniformR (0, (C8.length alphabet - 1)) gen
    return $ C8.singleton $ C8.index alphabet r1
  else do
    r1 <- MWC.uniform gen
    if r1
      then do
        r2 <- MWC.uniformR (0, C8.length ws - 1) gen
        let (pre, suf) = C8.splitAt r2 ws
        return $ pre `C8.append` C8.tail suf
      else do
        r2 <- MWC.uniformR (0, C8.length ws) gen
        r3 <- MWC.uniformR (0, (C8.length alphabet - 1)) gen
        let (pre, suf) = C8.splitAt r2 ws
        let wsc = C8.index alphabet r3
        return $ C8.append pre $ C8.cons wsc suf

whitespaceNeighbour :: forall m. PrimMonad m => MWC.Gen (PrimState m) -> Program -> m Program
whitespaceNeighbour gen (Program nodes names) = do
    r1 <- MWC.uniformR (0, (2 * numTokns nodes) - 1 ) gen
    Right newTree <- newNodes r1 nodes
    return $ Program newTree names
  where
    whitespace = C8.pack " \n"
    newNodes :: Int -> Tree -> m (Either Int Tree)
    newNodes n t@(NameLeaf _) = return $ Left n
    newNodes n (Node toks childs) = if n >= 2 * length toks
      then newNodes' (n - 2 * length toks) childs >>= \case
        Left n' -> return $ Left n'
        Right childs' -> return $ Right $ Node toks childs'
      else do
        let tokIdx = n `div` 2
        let side = n `mod` 2
        let pre = take tokIdx toks
        let (tok:suf) = drop tokIdx toks
        newTok <- if side == 0
          then do
            newFix <- changeFromAlphabet whitespace gen (prefix tok)
            return $ tok {prefix = newFix}
          else do
            newFix <- changeFromAlphabet whitespace gen (suffix tok)
            return $ tok {suffix = newFix}
        return $ Right $ Node (pre ++ (newTok : suf)) childs

    newNodes' :: Int -> [Tree] -> m (Either Int [Tree])
    newNodes' n [] = return $ Left n
    newNodes' n (x:xs) = newNodes n x >>= \case
        Left n' -> fmap (x :) <$> newNodes' n' xs
        Right x' -> return $ Right $ x' : xs

