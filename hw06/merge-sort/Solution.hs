module Solution where

solution:: [Int] -> [Int] -> [Int]

solution a b
    |a==[] = b
    |b==[] = a
    |(head a)<(head b) = (head a):(solution (tail a) b)
    |otherwise = (head b):(solution a (tail b))
