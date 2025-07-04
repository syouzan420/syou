module Keisan where

import Control.Monad (replicateM)
import Data.List (nub)
import Random (getRan,getRanList)
import Sosu (sobun)

data Kou = Kou {negate :: !Bool -- allow negative num
               ,negateP :: !Bool -- allow negative elements of product
               ,minNum :: !Int
               ,maxNum :: !Int
               ,pNum :: !Int -- number of multiplication
               ,dNum :: !Int -- number of division
               } deriving (Eq,Show)

data Siki = K !Kou | S !Kou !Siki | P !Kou !Siki   deriving (Eq,Show)
--K: One Kou, S: Kou and Siki, P: Kou and (Siki) (Kou may be in front or back of Siki)

siki :: Int -> IO (String,Int)
siki i = showSiki (makeSiki i)

showSiki :: Siki -> IO (String,Int)
showSiki s = do
  ((ss,sr),_) <- showSiki' s 
  let tk = take 1 ss
  let str = if tk == "＋" then drop 1 ss else ss
  putStrLn str 
  return (str,sr)

showSiki' :: Siki -> IO ((String,Int),Int)
showSiki' (K k) = showKou k
showSiki' (S k s) = do
  ((ks,kr),g) <- showKou k
  ((ss,sr),_) <- showSiki' s
  (wh,ng) <- getRan 2 g 
  if wh==0 then return ((ks++ss,kr+sr),ng) else return ((ss++ks,sr+kr),ng)


makeSiki :: Int -> Siki
makeSiki 0 = let k = makeKou 1 in S k (S k (K k))
makeSiki 1 = let k = makeKou 4 in S k (S k (S k (S k (K k))))
makeSiki 2 = let k = makeKou 5 in S k (S k (S k (S k (K k))))
makeSiki 3 = let k = makeKou 5; p = makeKou 6 in S k (S k (S p (S p (K k))))
makeSiki 4 = let k = makeKou 5; p = makeKou 7 in S k (S k (S p (S p (K k))))
makeSiki 5 = let k = makeKou 5; p = makeKou 9 in S k (S p (S p (K k)))

makeKou :: Int -> Kou
makeKou 0 = Kou False False 1 9 0 0   -- 5 +9 
makeKou 1 = Kou False False 1 99 0 0  -- 45 +67 
makeKou 2 = Kou False False 2 9 1 0   -- 4*6
makeKou 3 = Kou False False 2 99 1 0   -- 42*63
makeKou 4 = Kou True False 1 9 0 0    -- 3 -4
makeKou 5 = Kou True False 1 99 0 0   -- 36 -23
makeKou 6 = Kou True False 2 9 1 0    -- -2*3
makeKou 7 = Kou True False 2 99 1 0   -- -34*8 
makeKou 8 = Kou True False 2 99 0 1   -- -68/2
makeKou 9 = Kou True False 2 99 1 1   -- -98*5/2

getRanNums :: Int -> Bool -> Int -> Int -> Int -> IO [Int]
getRanNums i ispn miN maN g = do
  nums <- getRanList (maN-miN) i g
  nums2 <- getRanList maN i (g+50)
  let numRes = map (+miN) nums
  let mods = map (`mod` 2) nums2
  let res = zipWith (\n m -> if m==1 && ispn then -n else n) numRes mods
  return res

numsNegate :: Bool -> [Int] -> Int -> IO ([Int],Int)
numsNegate _ [] g = return ([],g)
numsNegate ispn (x:xs) g = do
  (neg,ng) <- getRan 2 g
  let negate = ispn && neg==1
      res = if negate then -x else x
  other <- numsNegate ispn xs ng
  return (res:fst other,ng)

showKou :: Kou -> IO ((String,Int),Int)
showKou (Kou isn ispn miN maN pn dn) = do 
  let ispr = pn>0 || dn>0
  nums <- getRanNums (pn+1) ispn miN maN 0  
  (neg,ng) <- getRan 2 0
  let negate = isn && neg==1  -- True なら 項自體が負になる
  let prod = product nums     -- かけ算部分をかけた結果
  let sbs = nub $ map fromIntegral $ sobun (fromIntegral prod)
                           -- かけ算の結果を素因数分解して重複しない數のリスト
  let isw = length sbs >= dn   -- 割算の數は因数の數を越えてゐないか
  let wcho = if ispr && isw then choose (length sbs) dn else [] 
  (chdi,ng2) <- if null wcho then return (0,ng) else getRan (length wcho) ng
  let numds = if null wcho then sbs else map (sbs !!) (wcho!!chdi)
                                                -- 割り算に使ふ數字のリスト
  let dn' = length numds                          --最終的な割り算の個数
  let choList = if not ispr then [] else choose (pn+dn') dn'  
  let lnCho = length choList 
  (pnm,ng3) <- if not ispr then return (0,ng2) else getRan lnCho ng2 
  let dnums = if null choList then [] else choList!!pnm 
      -- [0,2] ==> i.e. 3/4*5/6*7 [0,1] ==> i.e. 5/6/2*4*7 
  let res
        | length nums ==1 && not ispr = let n = head nums in if negate then -n else n 
        | otherwise = let n = foldl div prod numds in if negate then -n else n  
  let str
        | length nums ==1 && not ispr = let s = show $ head nums 
                             in if negate then "－"++s else "＋"++s 
        | otherwise = let s = showP nums numds dnums 
                       in if negate then "－"++s else "＋"++s 
  putStrLn str
  return ((str,res),ng3)

showP :: [Int] -> [Int] -> [Int] -> String 
showP nums numds dnums = 
  let lng = length nums + length numds - 1
      hd = show (head nums)
      ms = map (("×" ++).show) (tail nums) 
      ds = map (("÷" ++).show) numds
      dsi = zip dnums ds 
      msi = zip (filter (`notElem` dnums) [0..(lng-1)]) ms
   in hd++concatMap snd (sort (msi++dsi))


-- リストの要素がすべて異なる場合True
allDif :: Eq a => [a] -> Bool
allDif [] = True
allDif (x:xs) = notElem x xs && allDif xs 

-- choose a b : a個のものからb個選ぶ組み合はせ
choose :: Int -> Int -> [[Int]]
choose a c = nub $ map sort $ filter allDif $ replicateM c [0..(a-1)] 

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort smaller ++ [x] ++ sort larger
  where smaller = [a | a <-xs, a <= x]
        larger = [b | b <-xs, b > x]
