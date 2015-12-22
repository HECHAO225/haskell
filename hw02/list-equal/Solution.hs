module Solution where
solution :: [Integer] -> [Integer] -> Bool
solution xs ys
    | xs == [] && ys == [] = True
    | xs == [] = False
    | ys == [] = False
    | otherwise = (head xs == head ys)  && (solution (tail xs) (tail ys))
