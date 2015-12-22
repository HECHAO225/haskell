module Solution where
solution :: Integer -> Integer -> Integer
solution n k
    |(k <= 1) && (n < 10) = n
    |k >= 1 = solution ((solution n (-1)) * k) 1
    |k == -1 = (solution (div n 10) (-1)) + (mod n 10)
    |otherwise = 0
