module Hart.Annealing
    ( simulatedAnnealing
    , test
    ) where

import           System.Random (RandomGen, randomR, mkStdGen)

type Temp = Float
type E = Float

simulatedAnnealing
    :: (RandomGen g)
    => Int              -- number of steps
    -> (a -> [a])       -- neighbour function
    -> (a -> E)         -- energy function
    -> g -> a -> (a, g) -- it's random
simulatedAnnealing = simulatedAnnealingInternal acceptanceProbability linearCooling

simulatedAnnealingInternal
    :: (RandomGen g)
    => (E -> E -> Temp -> Float)
    -> (Int -> Int -> Temp)
    -> Int
    -> (a -> [a])
    -> (a -> E)
    -> g -> a -> (a, g)
simulatedAnnealingInternal ap temp n neigh e rg s0 = foldl (step neigh e ap) (s0, rg) (map (uncurry temp) $ zip (repeat n) [0..n])

step :: (RandomGen g)
     => (a -> [a])                -- neighbours of given state
     -> (a -> E)                  -- energy function
     -> (E -> E -> Temp -> Float) -- acceptance probability
     -> (a, g) -> Temp -> (a, g)  -- foldl
step neigh e ap (prev, rg) temp = let neighbours = neigh prev
                                      (i, rg') = randomR (0, length neighbours - 1) rg
                                      next = neighbours !! i
                                      (apv, rg'') = randomR (0.0, 1.0) rg'
                                      next' = if ap (e prev) (e next) temp >= apv
                                              then next else prev
                                   in (next', rg'')

acceptanceProbability :: E -> E -> Temp -> Float
acceptanceProbability prev next temp = if next < prev
                                       then 1.0 else exp $ (prev - next) / temp

linearCooling :: Int -> Int -> Temp
linearCooling a b = fromIntegral a / fromIntegral b

-- Just testing stuff
test :: Int -> Int -> Int -> Int -> Int -> Int -> (Int, [Int])
test zig zag size steps width seed =
    let rg = mkStdGen seed
        list = map (\i -> (i `div` zig) + (i `mod` zag)) [0..size]
        inrange i = i >= 0 && i < size
        neigh i = filter inrange $ [i-width..i-1] ++ [i+1..i+width]
        energy i = 1 / fromIntegral (list !! i)
        solution = fst $ simulatedAnnealing steps neigh energy rg 0
     in (solution, take 10 $ drop (solution - 5) list)

