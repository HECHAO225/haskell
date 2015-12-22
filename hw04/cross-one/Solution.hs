module Solution where
solution :: Int -> [a] -> [a] -> ([a], [a])

solution i xs ys = ((take i ys) ++ (drop i xs), (take i xs) ++ (drop i ys))
