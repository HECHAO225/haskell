module Solution where

import Data.List
import Test.QuickCheck
solution:: [Int] -> [Int] -> [Int]

solution a b
    |a==[] = b
    |b==[] = a
    |(head a)<(head b) = (head a):(solution (tail a) b)
    |otherwise = (head b):(solution a (tail b))


prop_sorted a b = and [flag|let list =(solution (sort a) (sort b)), i<-[0..((length list)-2)], let flag = (list!!i)<=(list!!(i + 1))]

prop_length a b = length (solution (sort a) (sort b)) == (length a) + (length b)
prop_sortedSystem a b = (solution (sort a) (sort b)) == (sort (a++b))
