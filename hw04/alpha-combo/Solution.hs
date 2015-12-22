module Solution where
import Data.List

solution :: Integer->[[Char]]
search :: Integer->[Char]->[[Char]]

solution n = [ys|z<-(tails ['a'..'z']), ys<-(search n z)]

search n "" = []
search n (x:xs) | (toInteger(length xs) < n-1)= []
                | n == 1 = [[x]]
                | otherwise= [x:ys|z<-(tails xs), ys<-(search (n-1) z)]

