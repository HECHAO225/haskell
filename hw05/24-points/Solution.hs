module Solution where
solution::[Int] -> Bool
answer::[Double] -> Bool
remove::[Double]->Int->Int->[Double]
caculate::Double->Double->[Double]

solution list = answer (map fromInteger list :: Double)

answer list
    |(length list)== 1 = if abs((head list)-24)<1e-6 then True else False
    |otherwise = or[x|xs <- (takeall list 0 1), let x = (answer xs)]

takeall list i j
    |i == (length list) - 1 = []
    |j >= (length list)= takeall list (i + 1) (i + 2)
    |otherwise =[(remove list i j)++[x]|x<-(caculate (list!!i) (list!!j))] ++ (takeall list i (j+1))

remove list i j
    |(length list) == 0 = []
    |(i/=0)&&(j/=0) = (head list):(remove (tail list) (i-1) (j-1))
    |otherwise = remove (tail list) (i-1) (j-1)
caculate x y
    |(x==0)|| (y==0) = [x+y, x*y, x-y, y-x]
    |otherwise = [x+y, x*y, x-y, y-x, x/y, y/x]
