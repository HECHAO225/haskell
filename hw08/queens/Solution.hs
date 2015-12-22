module Solution where

import Data.Char

solution :: [(Char, Int)]->Int

solution lists
        |null lists = 8
        |otherwise = getAnswer (getRemain lists [(i,j)| i<-[1..8], j<-[1..8]])

getRemain lists nodes
        |null lists = nodes
        |otherwise = getRemain (tail lists) (removeNode (ord (fst (head lists)) - 96,snd (head lists)) nodes)

getAnswer lists
        |null lists = 0
        |otherwise = (maximum [ans|list<-lists, let p1=(fst list), let q1=(snd list),
                      let ans = (getAnswer (removeNode list lists))]) + 1

removeNode node lists = [li|li<-lists, let p2=(fst li), let q2=(snd li),p2/=(fst node),
                         q2/=(snd node), (p2+q2)/=((fst node)+(snd node)), (p2-q2)/=(fst node)-(snd node)]