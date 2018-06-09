module Hart.Annealing
    ( simulatedAnnealing
    , test
    ) where

import           System.Random.MWC
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Control.Monad (foldM)

type Temp = Float
type E = Float

simulatedAnnealing
    :: (PrimMonad m)
    => Gen (PrimState m)
    -> Int        -- number of steps
    -> (Gen (PrimState m) -> a -> m a) -- neighbour function
    -> (a -> E)   -- energy function
    -> a -> m a   -- it's random
simulatedAnnealing gen = simulatedAnnealingInternal gen acceptanceProbability linearCooling

simulatedAnnealingInternal
    :: (PrimMonad m)
    => Gen (PrimState m)
    -> (E -> E -> Temp -> Float)
    -> (Int -> Int -> Temp)
    -> Int
    -> (Gen (PrimState m) -> a -> m a) -- neighbour function
    -> (a -> E)
    -> a -> m a
simulatedAnnealingInternal rg ap temp n neigh e s0 = foldM (step rg neigh e ap) s0 (map (uncurry temp) $ zip (repeat n) [0..n])

step :: (PrimMonad m)
     => Gen (PrimState m)
     -> (Gen (PrimState m) -> a -> m a)                -- neighbours of given state
     -> (a -> E)                  -- energy function
     -> (E -> E -> Temp -> Float) -- acceptance probability
     -> a -> Temp -> m a          -- foldM
step rg neigh e ap prev temp = do next <- neigh rg prev
                                  apv <- uniformR (0.0, 1.0) rg
                                  return $ if ap (e prev) (e next) temp >= apv
                                     then next
                                     else prev

acceptanceProbability :: E -> E -> Temp -> Float
acceptanceProbability prev next temp = if next < prev
                                       then 1.0 else exp $ (prev - next) / temp

linearCooling :: Int -> Int -> Temp
linearCooling a b = fromIntegral a / fromIntegral b

-- Just testing stuff
test :: Int -> Int -> Int -> Int -> Int -> IO (Int, [Int])
test zig zag size steps width =
    do rg <- create
       let list = map (\i -> (i `div` zig) + (i `mod` zag)) [0..size]
           energy i = 1 / fromIntegral (list !! i)
           neigh rg' i = uniformR (max 0 (width-i), min (size-1) (width+i)) rg'
       solution <- simulatedAnnealing rg steps neigh energy 0
       return (solution, take 10 $ drop (solution - 5) list)

