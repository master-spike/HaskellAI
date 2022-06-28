module SearchTest(Node, cost) where
import Search(treesearch)


data Node = A | B | C | D | E | F | G deriving (Read,Show,Eq,Enum)

cost A = [(D,2),(C,3),(B,9)]
cost B = [(A,9),(C,2)]
cost C = [(A,3),(B,2),(G,5)]
cost D = [(A,2),(F,2),(E,4)]
cost E = [(D,4),(F,2),(G,4)]
cost F = [(D,3),(E,2),(G,9)]
cost _ = []
