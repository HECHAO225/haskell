module Solution where
solution :: [Integer] -> [Integer] -> Integer
solution xs ys
    | xs == [] = toInteger (length ys)
    | ys == [] = toInteger (length xs)
    | (head xs) < (head ys) = 1 + solution (tail xs) ys
    | (head ys) < (head xs) = 1 + solution xs (tail ys)
    | (head ys) == (head xs) = solution (tail xs) (tail ys)
    | otherwise = 0