module Solution where
solution :: [Char] -> Maybe Integer

solution express
    |place_jj > 0 = getAnswer (express!!place_jj) (solution (take place_jj express)) (solution (drop (place_jj+1) express))
    |place_cc > 0 = getAnswer (express!!place_cc) (solution (take place_cc express)) (solution (drop (place_cc+1) express))
    |(head express)=='(' = solution (tail (take ((length express) -1) express))
    |otherwise = Just (read express::Integer)
    where place_jj = search_jiajian express ((length express)-1) 0
          place_cc = search_chengchu express ((length express)-1) 0

search_jiajian express num lay
    |num < 0 = -1
    |express!!num == '(' = search_jiajian express (num-1) (lay+1)
    |express!!num == ')' = search_jiajian express (num-1) (lay-1)
    |(express!!num == '+')&&(lay==0) = num
    |(express!!num == '-')&&(lay==0) = num
    |otherwise = search_jiajian express (num-1) lay
    
search_chengchu express num lay
    |num < 0 = -1
    |express!!num == '(' = search_chengchu express (num-1) (lay+1)
    |express!!num == ')' = search_chengchu express (num-1) (lay-1)
    |(express!!num == '*')&&(lay==0) = num
    |(express!!num == '/')&&(lay==0) = num
    |otherwise = search_chengchu express (num-1) lay

getAnswer signal Nothing _ = Nothing
getAnswer signal _ Nothing = Nothing
getAnswer signal (Just p) (Just q)
    |signal == '+' = Just ((+) p q)
    |signal == '-' = Just ((-) p q)
    |signal == '*' = Just ((*) p q)
    |(signal == '/')&&(q/=0) = Just (div p q)
    |signal == '/' = Nothing
