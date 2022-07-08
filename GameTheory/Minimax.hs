module GameTheory.Minimax(minimax) where

maxBy :: (Ord a) => (b -> a) -> [b] -> b
maxBy f [x]    = x
maxBy f (x:xs) = if f x > f y then x else y
    where y = maxBy f xs

minBy :: (Ord a) => (b -> a) -> [b] -> b
minBy f [x]    = x
minBy f (x:xs) = if f x < f y then x else y
    where y = minBy f xs

minimax :: (a -> [a]) -> (a -> Double) -> Int -> a -> Bool -> (Double, [a])
minimax succ eval d node maxP
    | d <= 0 || null (succ node) = (eval node, [node])
    | otherwise = (value, node:next) 
    where f        = if maxP then maxBy fst else minBy fst
          svals    = map (\x -> minimax succ eval (d-1) x (not maxP)) $ succ node
          (value,next) = f svals

