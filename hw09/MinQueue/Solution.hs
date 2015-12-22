module Solution where

data MinQueue a = Empty | Node a Int (MinQueue a) (MinQueue a)
                  deriving (Show, Eq, Ord)

empty = Empty

size Empty = 0
size (Node _ sum _ _) = sum

getMin Empty = Nothing
getMin (Node min _ _ _) = Just min

deleteMin Empty = Empty
deleteMin (Node min sum lch rch)
    |(lch == Empty)&&(rch == Empty) = Empty
    |lch == Empty = rch
    |rch == Empty = lch
    |lch < rch = Node minl (sum-1) (deleteMin lch) rch
    |otherwise = Node minr (sum-1) lch (deleteMin rch)
    where (Node minl suml _ _) = lch
          (Node minr sumr _ _) = rch

insert num Empty = Node num 1 Empty Empty
insert num (Node min sum lch rch)
    |lch == Empty = if (num < min) then (Node num (sum+1) (insert min lch) rch) else (Node min(sum+1) (insert num lch) rch)
    |rch == Empty = if (num < min) then (Node num (sum+1) lch (insert min rch)) else (Node min(sum+1) lch (insert num rch))
    |suml < sumr = if (num < min) then (Node num (sum+1) (insert min lch) rch) else (Node min(sum+1) (insert num lch) rch)
    |otherwise = if (num < min) then (Node num (sum+1) lch (insert min rch)) else (Node min(sum+1) lch (insert num rch))
    where (Node minl suml _ _) = lch
          (Node minr sumr _ _) = rch
