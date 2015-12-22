module Solution where
import Data.Char
solution :: [Char] -> [Char]
divtwo :: [Char] -> Int -> Int -> [Char]
solution d
    |d == "1" = "1"
    |mod (ord(d !! (length d -1))) 2== 0 = solution (divtwo d 0 0) ++ "0" 
    |mod (ord(d !! (length d -1))) 2== 1 = solution (divtwo d 0 0) ++ "1" 
    | otherwise = "0"

divtwo str now back
    |now == length str = ""
    |((str!!now)=='1') && (now ==0) = divtwo str (now+1) 1
    |otherwise =[chr((div (ord(str !! now) - 48 + (back * 10)) 2) + 48)] ++ (divtwo str (now+1) (mod (ord (str !! now)) 2))
