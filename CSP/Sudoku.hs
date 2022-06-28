module CSP.Sudoku(sudokuToBinaryCSP, assignmentToSolution) where
import CSP.CSPUtil
import CSP.BinaryCSPSolver
import Data.Array

--                   sqrtSize | (coords,val)  => BinaryCSP specification
sudokuToBinaryCSP :: Int -> [((Int,Int),Int)] -> BinaryCSP
sudokuToBinaryCSP sqrtSize preFilledSquares = (doms,ucs,bcs)
    where n    = sqrtSize * sqrtSize
          arcs = concat [ sudokuBoxConstraints sqrtSize,
                          sudokuVertConstraints n,
                          sudokuHoriConstraints n       ]
          bcs  = (genBlankBCSet (n*n))//[ (p, [(/=)]) | p <- arcs ]
          ucs  = (genBlankUCSet (n*n))//[ (x + y * n, [(==v)]) | ((x,y),v) <- preFilledSquares ]
          doms = genDomainsOfRange (n*n) 1 n
          
-- should pass sqrt size as arg
sudokuBoxConstraints  :: Int -> [(Int,Int)]
sudokuBoxConstraints sn = concat [allDistinctPairs (box (i,j)) | i <- [0..sn-1], j <- [0..sn-1]]
    where n = sn*sn
          box (i,j) = [ toIntCoord (x,y) | x <- [i*sn .. (i+1)*sn-1], y <- [j*sn .. (j+1)*sn-1]]
          toIntCoord (x,y) = x + y * n

sudokuVertConstraints :: Int -> [(Int,Int)]
sudokuVertConstraints n = concat [allDistinctPairs (col x) | x <- [0..n-1] ]
    where col x = [ x + y*n | y <- [0..n-1]]
    
sudokuHoriConstraints :: Int -> [(Int,Int)]
sudokuHoriConstraints n = concat [allDistinctPairs (row x) | x <- [0..n-1] ]
    where row x = [ y + x*n | y <- [0..n-1]]

allDistinctPairs :: (Eq a) => [a] -> [(a,a)]
allDistinctPairs xs = filter (\(x,y) -> x /= y) pairs
    where pairs = concat (map (\x -> (map (\y -> (x,y)) xs)) xs)
    
assignmentToSolution :: Int -> Maybe Assignment -> Maybe [((Int,Int),Int)]
assignmentToSolution _ Nothing = Nothing
assignmentToSolution n (Just assignment) = Just [ ((swap (i `divMod` n)),assignment!i) | i <- [0..n*n-1]]
    where swap = (\(x,y) -> (y,x))
    
