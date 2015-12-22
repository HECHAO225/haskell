module Solution where
solution::Integer->Integer
solution n
    |n==0 = 0
    |n==1 = 1
    |otherwise = answer 0 1 1 (n-2)
answer a0 a1 a2 n
    |n==0 = a2
    |otherwise = answer a1 a2 (a0+a1+a2) (n-1)