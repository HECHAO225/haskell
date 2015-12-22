module Solution where
solution :: Integer -> Integer -> Integer -> Integer
solution n m t
    |(mod t (gcd n m)) /= 0 = -1
    |otherwise = min(getanswer 0 0 n m t) (getanswer 0 0 m n t)

getanswer x y n m t
    |n < t = 100000
    |x == t = 0
    |y == m = (getanswer x 0 n m t) + 1
    |x == 0 = (getanswer n y n m t) + 1
    |otherwise = (getanswer (max 0 (x-(m-y))) (min m (y+x)) n m t) + 1

