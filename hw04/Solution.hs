module Solution where
import Data.Char
solution :: Integer->[[Char]]
search :: Integer->Char->[[Char]]
solution n = search n 'a'

search n ch
    |n == 0 = [[]]
    |ch > 'z' = []
    |toInteger(ord('z')-ord(ch)+1) < n = []
    |otherwise = (addList ch (search (n-1) (chr(ord(ch)+1)))) ++ (search n (chr(ord(ch)+1)))

addList ch list
    |list == [] = []
    |otherwise = (ch:(head list)) : (addList ch (tail list))
