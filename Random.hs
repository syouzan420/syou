module Random (getRandomNumIO,getRandomNum,getRan,getRanList) where

import Data.Time.Clock(getCurrentTime,UTCTime(utctDayTime))
import Browser (getRanNum)

-- get random number(Int) from 0 to (i-1)
-- input (Number,Generator) -> output (Number,Generator)
getRandomNumIO :: (Int,Int) -> IO (Int,Int)
getRandomNumIO (i,g) = do
  utime <- getCurrentTime 
  let time = show (utctDayTime utime)
  let ntime = init time
  let n = (read$tail$dropWhile (/='.') ntime)::Int
  let ng = (read$take 5 (show (n+g*(i+1))))::Int
  let m = mod ng i
  return (m,ng)

getRandomNum :: (Int,Int) -> (Int,Int)
getRandomNum (i,g) =
  let ng = (read$take 3 (show (g*(i-1))))::Int
      m = mod ng i
   in (m,ng)

--
getRan :: Int -> IO Int
getRan = getRanNum 
--getRan i g = getRandomNumIO (i,g)

-- getRanList m i g , m:length of list from which, i:take i of the list
getRanList :: Int -> Int -> IO [Int]
getRanList m = getRanList2 [0..(m-1)] 

--getRanList' :: [Int] -> Int -> Int -> IO [Int]
--getRanList' _ 0 _ = return []
--getRanList' il i g = do
--  (ti,g') <- getRan (length il) g
--  let num = il!!ti
--  xs <- getRanList' (filter (/=num) il) (i-1) g'
--  return (num:xs)

getRanList2 :: [Int] -> Int -> IO [Int]
getRanList2 _ 0 = return []
getRanList2 il i = do
  ti <- getRan (length il)
  let num = il!!ti
  xs <- getRanList2 (filter (/=num) il) (i-1) 
  return (num:xs)
