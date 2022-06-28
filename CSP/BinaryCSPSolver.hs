module BinaryCSPSolver(forwardChecking) where

import CSPUtil
import Data.Array


fwchkFilt :: BinaryCSP -> Int -> Int -> BinaryCSP
fwchkFilt (doms,uc,bc) var val = (ndoms,uc,bc)
    where (l,u)  = bounds doms
          ndoms  = doms//[(i, filter (\d -> (allsat d val (bc!(i,var))) && (allsat val d (bc!(var,i)))) (doms!i)) | i <- [l..u]]
          allsat a b []     = True
          allsat a b (c:cs) = (c a b) && (allsat a b cs) 


-- if d in doms!i then c d val for all c in bc!(i,var)
--                 and c val d for all c in bc!(var,i) 

removeFromDomain :: BinaryCSP -> Int -> Int -> BinaryCSP
removeFromDomain (doms,uc,bc) var val = (doms//[(var, filter (/= val) (doms!var))],uc,bc)

hasEmptyDomain :: Domains -> Bool
hasEmptyDomain = elem []

nextAssignment :: Domains -> PartialAssignment -> (Int,Int)
nextAssignment domains pa = (var, head (domains!var))
    where findNothing ((_,Just _):xs) = findNothing xs
          findNothing ((i,Nothing):_) = i
          var = findNothing (assocs pa)
          
    
assign :: Domains -> PartialAssignment -> Int -> Int -> (Domains,PartialAssignment)
assign doms pa var val = (ndoms,npa)
    where ndoms = doms//[(var,[val])]
          npa   = pa//[(var,Just val)]

satisfyUCS :: Domains -> UnaryConstraintSet -> Domains
satisfyUCS doms ucs = doms//[(i, filter (allsat (ucs!i)) (doms!i)) | i <- [l..u]]
    where (l,u) = bounds doms
          allsat (c:cs) x = (c x) && (allsat cs x)
          allsat []     _ = True

forwardChecking :: BinaryCSP -> Maybe Assignment
forwardChecking (domains,ucs,bcs) = fwchk (satisfyUCS domains ucs,ucs,bcs) (genEmptyPartialAssignment (domains,ucs,bcs))
    where firstSomething Nothing x  = x
          firstSomething (Just x) _ = Just x
          fwchk (dom,uc,bc) pa
           | paIsComplete pa    = Just (paToFullA pa)
           | hasEmptyDomain dom = Nothing
           | otherwise          = firstSomething (fwchk (fwchkFilt (ndom,uc,bc) var val) npa)
                                                (fwchk (removeFromDomain (dom,uc,bc) var val) pa)
              where (var,val)   = nextAssignment dom pa
                    (ndom,npa)  = assign dom pa var val
