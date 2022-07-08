module GameTheory.TicTacToe(TicTac,TTRow,TTBoard,solveTicTacToe) where

import GameTheory.Minimax

data TicTac = E | X | O deriving Show
type TTRow  = (TicTac,TicTac,TicTac)
type TTBoard = (TTRow,TTRow,TTRow)

emptyBoard :: TTBoard
emptyBoard = (er,er,er) where er = (E,E,E)

eval :: TTBoard -> Double
eval ((X,X,X),_,_) =  1.0
eval ((O,O,O),_,_) = -1.0
eval (_,(X,X,X),_) =  1.0
eval (_,(O,O,O),_) = -1.0
eval (_,_,(X,X,X)) =  1.0
eval (_,_,(O,O,O)) = -1.0
eval ((X,_,_),(X,_,_),(X,_,_)) =  1.0
eval ((O,_,_),(O,_,_),(O,_,_)) = -1.0
eval ((_,X,_),(_,X,_),(_,X,_)) =  1.0
eval ((_,O,_),(_,O,_),(_,O,_)) = -1.0
eval ((_,_,X),(_,_,X),(_,_,X)) =  1.0
eval ((_,_,O),(_,_,O),(_,_,O)) = -1.0
eval ((X,_,_),(_,X,_),(_,_,X)) =  1.0
eval ((O,_,_),(_,O,_),(_,_,O)) = -1.0
eval ((_,_,X),(_,X,_),(X,_,_)) =  1.0
eval ((_,_,O),(_,O,_),(O,_,_)) = -1.0
eval _ = 0.0

countXOboard (r1,r2,r3) = (r1x + r2x + r3x, r1o + r2o + r3o)
    where (r1x,r1o) = countXOrow r1
          (r2x,r2o) = countXOrow r2
          (r3x,r3o) = countXOrow r3

countXOrow (s1, s2, s3) = (f s1 + f s2 + f s3, g s1 + g s2 + g s3)
    where f x = case x of
                     X -> 1
                     _ -> 0
          g x = case x of
                     O -> 1
                     _ -> 0
    
    
succs board@(r1,r2,r3)
    | eval board > 0.9 || eval board < -0.9 = []
    | otherwise = f (repEs r1) ++ g (repEs r2) ++ h (repEs r3)
    where repEs (E,E,E) = [(p,E,E),(E,p,E),(E,E,p)]
          repEs (a,E,E) = [(a,p,E),(a,E,p)]
          repEs (E,a,E) = [(p,a,E),(E,a,p)]
          repEs (E,E,a) = [(p,E,a),(E,p,a)]
          repEs (E,a,b) = [(p,a,b)]
          repEs (a,E,b) = [(a,p,b)]
          repEs (a,b,E) = [(a,b,p)]
          repEs (a,b,c) = []
          f = map (\x -> (x,r2,r3))
          g = map (\x -> (r1,x,r3))
          h = map (\x -> (r1,r2,x))
          p = if x <= o then X else O
          (x,o) = countXOboard board

solveTicTacToe = minimax succs eval 9 emptyBoard True
