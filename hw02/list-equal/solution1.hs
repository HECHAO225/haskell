module Solution where
solution :: [Integer] -> [Integer] -> Bool
solution a b 
   |a==[] && b== [] = True
   |a==[] = False
   |b==[] = False
   |head a == head b = solution (tail a) (tail b)

