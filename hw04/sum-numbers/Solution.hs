module Solution where
solution ::String->Int
getNum :: String->Int
checkNum :: String->Int
solution str
    |str == "" = 0
    |(str!!0)==' ' = solution (dropWhile (==' ') str)
    |otherwise = getNum (takeWhile (/=' ') str) + solution (dropWhile (/=' ') str)

getNum str
    |(checkNum str) == 1 = 0
    |otherwise = read str::Int

checkNum str
    |str == "" = 0
    |('0' <= (str!!0))&&((str!!0)<='9') = checkNum (tail str)
    |otherwise = 1