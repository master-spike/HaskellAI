module Search.Search(treesearch,graphsearch) where
import qualified Data.HashSet as HashSet
import Data.Hashable(Hashable)

treesearch :: a -> (a -> Bool) -> (a -> [(a,Int)]) -> (([a],Int) -> [([a],Int)] -> [([a],Int)]) -> Maybe ([a],Int)

treesearch start goaltest succFn frontierIns =
    ts [([start], 0)]
    where
      ts [] = Nothing
      ts ((p,c):ns)
        | goaltest (head p) = Just (p,c)
        | otherwise = ts (foldr frontierIns ns nextnodes)
                    where nextnodes = map (\(x,y) -> (x:p,c+y)) (succFn (head p))
        
graphsearch :: (Eq a, Hashable a) => a -> (a -> Bool) -> (a -> [(a,Int)]) -> (([a],Int) -> [([a],Int)] -> [([a],Int)]) -> Maybe ([a],Int)
graphsearch start goaltest succFn frontierIns =
    gs [([start], 0)] HashSet.empty
    where
      gs [] _ = Nothing
      gs ((p,c):ns) visited
        | goaltest (head p) = Just (p,c)
        | otherwise = gs (foldr frontierIns ns nextnodes) (HashSet.insert (head p) visited)
                    where nextnodes = map (\(x,y) -> (x:p,c+y)) (filter (\(x,y) -> not (x `HashSet.member` visited)) ((succFn.head) p))
