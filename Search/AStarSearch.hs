module AStarSearch(astartree,astargraph) where
import Search(treesearch,graphsearch)


pqinsert :: (a -> Int) -> a -> [a] -> [a]
pqinsert f y [] = [y]
pqinsert f y (x:xs)
    | f(y) <= f(x) = y:x:xs
    | otherwise    = x:(pqinsert f y xs)

astartree :: a -> (a -> Bool) -> (a -> [(a,Int)]) -> (a -> Int) -> Maybe ([a],Int)
astartree start goaltest succFn heuristic = 
    treesearch start goaltest succFn (pqinsert f)
    where f (p,c) = heuristic (head p) + c
    
astargraph :: (Eq a) => a -> (a -> Bool) -> (a -> [(a,Int)]) -> (a -> Int) -> Maybe ([a],Int)
astargraph start goaltest succFn heuristic = 
    graphsearch start goaltest succFn (pqinsert f)
    where f (p,c) = heuristic (head p) + c
