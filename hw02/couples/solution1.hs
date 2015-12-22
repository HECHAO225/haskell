module Solution where
solution :: [Integer] -> [Integer] -> Integer
solution a b 
  |a==[ ] = toInteger (length b)
  |b==[ ] = toInteger (length a)
  |(head a == head b) = solution (tail a) (tail b)
  |(head a > head b) = 1 + solution a (tail b)
  |(head a < head b) = 1 + solution (tail a) b
  |otherwise = 0
