module Search.AStarTest(solvemazetree,solvemazegraph) where
import Search.AStarSearch(astartree,astargraph)


ups = [[],[0,1,2,3,4],[0,5,6,7],[2,6,7],[3,4,5,6,7],[0,1,2,4,5,6],[0,3,4,5,7],[1,2,3,6,7]]
downs = [[0,1,2,3,4],[0,5,6,7],[2,6,7],[3,4,5,6,7],[0,1,2,4,5,6],[0,3,4,5,7],[1,2,3,6,7],[]]
lefts = [[1,3,5,6,7],[2,4,6,7],[1,2,3,4,5],[1,2,3,5],[1,3],[2,3,7],[1,4,7],[1,3,5,6]]
rights = [[0,2,4,5,6],[1,3,5,6],[0,1,2,3,4],[0,1,2,4],[0,2],[1,2,6],[0,3,6],[0,2,4,5]]

initsq = (0,3)
goalsq = (7,4)

cellinlist (x,y) zss = y `elem` (zss !! x)

uneigh (x,y)
    | cellinlist (x,y) ups = [((x-1,y),1)]
    | otherwise = []

dneigh (x,y)
    | cellinlist (x,y) downs = [((x+1,y),1)]
    | otherwise = []

lneigh (x,y)
    | cellinlist (x,y) lefts = [((x,y-1),1)]
    | otherwise = []

rneigh (x,y)
    | cellinlist (x,y) rights = [((x,y+1),1)]
    | otherwise = []

succFn p = uneigh p ++ dneigh p ++ lneigh p ++ rneigh p

hamiltondist (a,b) (c,d) = (abs (a - c)) + (abs (b - d))

solvemazetree = astartree initsq (==goalsq) succFn (hamiltondist goalsq)
solvemazegraph = astargraph initsq (==goalsq) succFn (hamiltondist goalsq)
