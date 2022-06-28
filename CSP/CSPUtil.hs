module CSPUtil(BinaryConstraint, UnaryConstraint, UnaryConstraintSet, BinaryConstraintSet,
               Domains, BinaryCSP, Assignment, PartialAssignment, validBinAssignment,
               genBlankUCSet, genBlankBCSet, genDomainsOfRange, genEmptyPartialAssignment,
               addUConstraint, addBConstraint, paIsComplete, paToFullA) where

import Data.Array

type BinaryConstraint = Int -> Int -> Bool
type UnaryConstraint  = Int -> Bool

type BinaryConstraintSet = Array (Int,Int) [BinaryConstraint]
type UnaryConstraintSet = Array Int [UnaryConstraint]
type Domains = Array Int [Int]

type BinaryCSP = (Domains, UnaryConstraintSet, BinaryConstraintSet)
type Assignment = Array Int Int
type PartialAssignment = Array Int (Maybe Int)

genBlankUCSet :: Int -> UnaryConstraintSet
genBlankUCSet n = array (1,n) [(i,[]) | i <- [1..n]]

genBlankBCSet :: Int -> BinaryConstraintSet
genBlankBCSet n = array ((1,1),(n,n)) [((i,j),[]) | i <- [1..n], j <- [1..n]]

genDomainsOfRange :: Int -> Int -> Int -> Domains
genDomainsOfRange n lower upper = array (1,n) [(i,[lower..upper]) | i <- [1..n]]

genEmptyPartialAssignment :: BinaryCSP -> PartialAssignment
genEmptyPartialAssignment (domains,_,_) = listArray (bounds domains) [ Nothing | i <- [1..] ]

addUConstraint :: UnaryConstraintSet -> UnaryConstraint -> Int -> UnaryConstraintSet
addUConstraint ucs c i = ucs//[(i, c:(ucs!i))]

addBConstraint :: BinaryConstraintSet -> BinaryConstraint -> Int -> Int -> BinaryConstraintSet
addBConstraint bcs c i j = bcs//[((i,j), c:(bcs!(i,j)))]

paIsComplete :: PartialAssignment -> Bool
paIsComplete = testall . elems
    where testall []            = True
          testall ((Just _):xs) = testall xs
          testall (Nothing:_)   = False

paToFullA :: PartialAssignment -> Assignment
paToFullA pa = array (bounds pa) (map removejust (assocs pa))
    where removejust (i,(Just x)) = (i,x)

testDomains :: Domains -> Assignment -> Bool
testDomains domains assignment = if (bounds assignment) == (bounds domains)
                                 then testdoms l else False
     where (l,u) = bounds assignment
           testdoms i
             | i > u = True
             | (assignment!i) `notElem` (domains!i) = False
             | otherwise = testdoms (i+1)
     

testUnaryConstraints :: UnaryConstraintSet -> Assignment -> Bool
testUnaryConstraints uconstraints assignment = testucs l
     where (l,u) = bounds uconstraints
           testlist cs val = not (foldr (\c v -> v && (c val)) True cs)
           testucs i
             | i > u                                    = True
             | testlist (uconstraints!i) (assignment!i) = False
             | otherwise                                = testucs (i+1)
             
testBinaryConstraints :: BinaryConstraintSet -> Assignment -> Bool
testBinaryConstraints biconstraints assignment = testbcs lx ly
     where ((lx,ly),(ux,uy)) = bounds biconstraints
           testlist cs va vb = not (foldr (\c v -> v && (c va vb)) True cs)
           testbcs i j
             | i > ux                                                       = True
             | j > uy                                                       = testbcs (i+1) ly
             | testlist (biconstraints!(i,j)) (assignment!i) (assignment!j) = False
             | otherwise                                                    = testbcs i (j+1)


validBinAssignment :: BinaryCSP -> Assignment -> Bool
validBinAssignment (domains, ucs, bcs)
      = (\x -> (testDomains domains x)
               && (testUnaryConstraints ucs x)
               && (testBinaryConstraints bcs x))
    
    
    
    
