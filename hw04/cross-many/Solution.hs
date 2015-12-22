module Solution where
solution::[Int]->[a]->[a]->([a],[a])
getAnswer :: [Int] -> ([a], [a]) -> ([a], [a])
swapElement :: Int -> ([a], [a]) -> ([a], [a])
getShortLength :: [a] -> [a] -> Int
solution list xs ys = getAnswer list ((take (getShortLength xs ys) xs), (take (getShortLength xs ys) ys))

getAnswer list (xs, ys)
    |list == [] = (xs, ys)
    |(head list)>= (length xs) = (xs, ys)
    |otherwise = swapElement (head list) (getAnswer (tail list) (xs, ys))

swapElement i (xs, ys)
    |i < 0 = (xs, ys)
    |otherwise = ((take i xs) ++ [ys!!i] ++ (drop (i + 1) xs), (take i ys) ++ [xs!!i] ++ (drop (i+1) ys))

getShortLength [] _ = 0
getShortLength _ [] = 0
getShortLength (_:xs) (_:ys) = 1 + getShortLength xs ys
