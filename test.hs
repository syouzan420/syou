toMust :: [Int] -> [Int] -> [Int]
toMust lst [] = lst
toMust lst tg@(x:xs)
  | x `elem` lst = toMust lst xs
  | otherwise = let lng = length lst 
                    tNum = getRan lng
                    nlst = repList tNum x lst
                 in toMust nlst xs

getRan :: Int -> Int
getRan _ = 0

repList :: Int -> a -> [a] -> [a]
repList ind tg lst =
  take ind lst ++ [tg] ++ drop (ind+1) lst
  
