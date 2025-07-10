module Libs where

import Data.Array (listArray, (!))
import Data.Monoid ((<>))
import Random (getRan)

isChar :: String -> Bool
isChar = foldr (\x -> (&&) (x `elem` ['a'..'z']++['A'..'Z'])) True

isNum :: String -> Bool
isNum = foldr (\x -> (&&) (x `elem` "0123456789+-")) True

getIndex :: Eq a => a -> [a] -> Int
getIndex _ [] = 0
getIndex t (x:xs) = if t==x then 0 else 1+ getIndex t xs

sepByChar :: Char -> String -> [String]
sepByChar _ [] = [[]]
sepByChar _ [x] = [[x]]
sepByChar ch (x:xs)
  | ch==x = []:(hd:tl)
  | otherwise = (x:hd):tl
  where (hd:tl) = sepByChar ch xs

getInside :: Char -> Int -> String -> String
getInside ch mc msg = drop (mc+1) (getToChar ch (mc+1) msg) 
  where getToChar _ _ [] = []
        getToChar ch mc msg
          | msg!!mc==ch = take mc msg
          | otherwise = getToChar ch (mc+1) msg

concatWith :: Char -> [String] -> String
concatWith ch lst = init$concatMap (++[ch]) lst

repList :: Int -> a -> [a] -> [a]
repList i tg lst = take i lst ++ [tg] ++ drop (i+1) lst

insToList :: Int -> a -> [a] -> [a]
insToList i tg lst = take i lst ++ [tg] ++ drop i lst

delFromList :: Int -> [a] -> [a]
delFromList i ls = if length ls < i+1 then ls else take i ls <> drop (i+1) ls

selectData :: Int -> [a] -> IO [a]
selectData 0 _ = return []
selectData i rdt = do
  let maxI = length rdt - 1
  rn <- getRan (maxI+1)
  let rdtA = listArray (0,maxI) rdt
      dt = rdtA!rn
      dts = delFromList rn rdt
  sdts <- selectData (i-1) dts
  return (dt:sdts)

getAns :: String -> String
getAns str =
  let res =  concatMap (\(i,w) ->
                   if even i then w else
                     if '-' `elem` w then (tail . snd) (break (=='-') w) else "")
                                                   (zip [0..] (words str)) 
   in "<"++res++">"

getMon :: String -> String
getMon str =
  let res = concatMap snd $ filter (\(i,_) -> odd i) (zip [0..] (words str)) 
   in "<"++removeChr '-' res++">"

removeChr :: Char -> String -> String
removeChr _ [] = []
removeChr ch (x:xs) = if ch==x then xs else x:removeChr ch xs 

getTgt :: String -> (String,String)
getTgt [] = ([],[])
getTgt (x:xs)
  | x=='>' = ([],xs)
  | otherwise = (x:(fst (getTgt xs)),snd (getTgt xs))

toMon :: String -> (String,String)
toMon [] = ([],[])
toMon (x:xs)
  | x=='<' = let (tgt,xxs) = getTgt xs
                 nxt = toMon xxs
              in (getMon tgt++fst nxt,getAns tgt++snd nxt)
  | otherwise = let nxt = toMon xs
                 in (x:fst nxt,x:snd nxt) 

