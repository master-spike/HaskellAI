module LocalSearch.TravellingSalesman() where

import LocalSearch

-- TSP is number of locations and a distance for each location pair
type TSP a =  (Int, Int -> Int -> a)

type TSPSolution = [Int]

feasibleSolution :: (Ord a, Num a) => TSP a -> TSPSolution -> Bool
feasibleSolution (n,_) xs = False `notElem` [ i `elem` xs | i <- [0..n-1]] && length xs == n

evalSolution :: (Ord a, Num a) => TSP a -> TSPSolution -> Maybe a
evalSolution tsp@(n,f) (x:xs)
    | not (feasibleSolution tsp xs) = Nothing
    | otherwise = Just (loopsum x (x:xs))
    where loopsum z (x:y:xs) = f x y + loopsum z xs
          loopsum z [x]      = f x z
          
successors :: TSPSolution -> [TSPSolution]
successors xs = concat [ [    (take i xs)
                           ++ ((reverse . take j . drop i) xs)
                           ++ (drop (i+j) xs)
                         | j <- [2..n-i] ]
                         | i <- [0..n-2] ]
    where n = length xs
    

