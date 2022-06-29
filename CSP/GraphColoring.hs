module CSP.GraphColoring(graphToBinaryCSP, solveGraphColoring) where

import CSP.CSPUtil
import CSP.BinaryCSPSolver
import Data.Array

type GraphArcList = [(Int,Int)]
type Graph = (Int,GraphArcList) --(NumVertices, ListOfEdges)
                       
graphToBinaryCSP :: Int   -> -- max colours
                    Graph -> -- graph
                    BinaryCSP

graphToBinaryCSP nc (nv,arcs) = (genDomainsOfRange nv 1 nc, genBlankUCSet nv, (genBlankBCSet nv)//[(arc, [(/=)]) | arc <- arcs] )

solveGraphColoring :: Int -> Graph -> Maybe [(Int,Int)]
solveGraphColoring nc graph = return graph
                          >>= return.(graphToBinaryCSP nc)
                          >>= forwardChecking
                          >>= return.assocs
