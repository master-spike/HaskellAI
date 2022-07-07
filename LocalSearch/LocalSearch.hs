module LocalSearch(hillClimbing, bestNeighbour) where



hillClimbing :: Integer                  -- maximum iterations
                -> (a -> [a])            -- successor function
                -> (a -> [a] -> Maybe a) -- choice function over successors
                -> a                     -- initial solution we search from
                -> a                     -- output solution

hillClimbing n succs choice init
    | n == 0    = init
    | otherwise = case next of
                       Nothing    -> init
                       Just next' -> hillClimbing (n-1) next' succs choice
    where next = choice init . succs $ init
    
bestNeighbour :: (Ord b) => (a -> b)
                         -> a
                         -> [a]
                         -> Maybe a

bestNeighbour _ s []     = Nothing
bestNeighbour v s (x:xs)
    | v s > v x = bestNeighbour v s xs
    | otherwise = case bestNeighbour v x xs of
                       Nothing -> Just x
                       Just y  -> Just y
