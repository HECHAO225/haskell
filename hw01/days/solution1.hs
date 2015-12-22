module Solution where
solution:: Int -> Int -> Int
solution y m = case m of
       1 -> 31
       2 -> if mod y 4 == 0 && (mod y 100 /= 0 || mod y 400 == 0) then 29 else 28
       3 -> 31
       4 -> 30
       5 -> 31
       6 -> 30
       7 -> 31
       8 -> 31
       9 -> 30
       10 -> 31
       11 -> 30
       12 -> 31
       _ -> error "invalid month"