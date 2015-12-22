module Solution where
solution :: [(Int, Int)]->Rational
cross :: (Int, Int)->(Int, Int)->Int
minus :: (Int, Int)->(Int, Int)->(Int, Int)

solution list = abs(toRational (sum [x|i<-[1..((length list)-2)], let x = (cross (minus (list!!i) (list!!0)) (minus (list!!(i+1)) (list!!0)))])/ 2)

cross p q = (fst p) * (snd q) - (snd p) * (fst q)

minus p q = (fst p - fst q, snd p - snd q)
