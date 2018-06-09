import qualified System.Random.MWC as MWC

import Tree


changeFromAlphabet :: PrimMonad m => C8.ByteString -> Gen (PrimState m) -> C8.ByteString -> m C8.ByteString
changeFromAlphabet alphabet gen ws = if C8.null ws
  then do
    r1 <- MWC.uniformR 0 (C8.length alphabet - 1)
    return $ C8.index alphabet r1
  else do
    r1 <- MWC.uniform
    if r1
      then do
        r2 <- MWC.uniformR (0, C8.length ws - 1)
        let (pre, suf) = C8.splitAt r2 ws
        return $ pre `C8.append` C8.tail suf
      else do
        r2 <- MWC.uniformR (0, C8.length ws)
        r3 <- MWC.uniformR 0 (C8.length alphabet - 1)
        let (pre, suf) = C8.splitAt r2 ws
        let wsc = C8.index alphabet r3
        return $ C8.append pre $ C8.cons wsc suf

whitespaceNeighbour :: PrimMonad m => Gen (PrimState m) -> Program -> m Program
whitespaceNeighbour gen (Program nodes names) = do
    r1 <- MWC.uniformR (0, (2 * numTokns nodes) - 1 )
    return $ Program newNodes names
  where
    newNodes :: Int -> Tree -> m (Either Int Tree)
    newNodes n t@(NameLeaf _) = return $ Left n
    newNodes n (Node toks childs) = if n >= 2 * length toks
      then case newNodes' (n - 2 * length toks) childs of
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
      
    newNodes' n [] = (n, [])
    newNodes' n (x:xs) = case newNodes n x of
        Left n' -> (x :) <$> newNodes n' xs
        Right x' -> x' : xs
      

