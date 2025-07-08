module Keisan2 where

import Control.Monad (replicateM)
import Data.List (nub)
import Random (getRan,getRanList)
import Sosu (sobun)

data Ko = Ko {negate :: !Bool -- allow negative num
             ,negateP :: !Bool -- allow negative elements of product
             ,minNum :: !Int
             ,maxNum :: !Int
             ,pNum :: !Int -- number of multiplication
             ,dNum :: !Int -- number of division
             } deriving (Eq,Show)

data Kou = A !Ko | B !Ko !Siki deriving (Eq,Show)

data Siki = K !Kou | S !Kou !Siki | P !Kou !Siki   deriving (Eq,Show)
--K: One Kou, S: Kou and Siki, P: Kou and (Siki) (Kou may be in front or back of Siki)

siki :: Int -> IO (String,Int)
siki i = showSiki (makeSiki i)

showSiki :: Siki -> IO (String,Int)
showSiki s = do
  (ss,sr) <- showSiki' s 
  let tk = take 1 ss
  let str = if tk == "＋" then drop 1 ss else ss
  putStrLn str 
  return (str,sr)

showSiki' :: Siki -> IO (String,Int)
showSiki' (K k) = showKou k
showSiki' (S k s) = do
  (ks,kr) <- showKou k
  (ss,sr) <- showSiki' s
  wh <- getRan 2 
  if wh==0 then return (ks++ss,kr+sr) else return (ss++ks,sr+kr)


makeSiki :: Int -> Siki
makeSiki 0 = let k = makeKou 1 in S k (S k (K k))
makeSiki 1 = let k = makeKou 4 in S k (S k (S k (S k (K k))))
makeSiki 2 = let k = makeKou 5 in S k (S k (S k (S k (K k))))
makeSiki 3 = let k = makeKou 5; p = makeKou 6 in S p (K k)
makeSiki 4 = let k = makeKou 5; p = makeKou 6 in S k (S k (S p (S p (K k))))
makeSiki 5 = let k = makeKou 5; p = makeKou 7 in S k (S k (S p (S p (K k))))
makeSiki 6 = let k = makeKou 5; p = makeKou 9 in S k (S p (S p (K k)))
makeSiki 7 = let k= makeKou 10; l = makeKou 9; m = makeKou 1 in S k (S l (K m))

makeKou :: Int -> Kou
makeKou 10 = B (makeKo 1) (makeSiki 3)
makeKou 11 = B (makeKo 1) (makeSiki 4) 
makeKou n = A (makeKo n) 

makeKo :: Int -> Ko
makeKo 0 = Ko False False 1 9 0 0   -- 5 +9 
makeKo 1 = Ko False False 1 99 0 0  -- 45 +67 
makeKo 2 = Ko False False 2 9 1 0   -- 4*6
makeKo 3 = Ko False False 2 99 1 0   -- 42*63
makeKo 4 = Ko True False 1 9 0 0    -- 3 -4
makeKo 5 = Ko True False 1 99 0 0   -- 36 -23
makeKo 6 = Ko True False 2 9 1 0    -- -2*3
makeKo 7 = Ko True False 2 99 1 0   -- -34*8 
makeKo 8 = Ko True False 2 99 0 1   -- -68/2
makeKo 9 = Ko True False 2 99 1 1   -- -98*5/2

getRanNums :: Int -> Bool -> Int -> Int -> IO [Int]
getRanNums i ispn miN maN = do
  nums <- getRanList (maN-miN) i
  nums2 <- getRanList maN i
  let numRes = map (+miN) nums
  let mods = map (`mod` 2) nums2
  let res = zipWith (\n m -> if m==1 && ispn then -n else n) numRes mods
  return res

numsNegate :: Bool -> [Int] -> IO [Int]
numsNegate _ [] = return []
numsNegate ispn (x:xs) = do
  neg <- getRan 2 
  let negate = ispn && neg==1
      res = if negate then -x else x
  other <- numsNegate ispn xs
  return (res:other)

showKou :: Kou -> IO (String,Int)
showKou (A ko) = showKouA ko 
showKou (B ko si) = do
  whi <- getRan 2  -- 項*式 にするか 式*項にするか二択
  let maheKou = whi==0
  (kouStr,kouRes) <- showKouA ko
  (sikiStr,sikiRes) <- showSiki si 
  let kouStr' = if kouRes<0 then "("++kouStr++")" else tail kouStr
  let str = if maheKou then kouStr++"×("++sikiStr++")"
                       else "＋("++sikiStr++")×"++kouStr'
  let res = kouRes*sikiRes
  return (str,res)

showKouA :: Ko -> IO (String,Int)
showKouA (Ko isn ispn miN maN pn dn) = do 
  let ispr = pn>0 || dn>0
  nums <- getRanNums (pn+1) ispn miN maN  
  neg <- getRan 2
  let negate = isn && neg==1  -- True なら 項自體が負になる
  let prod = product nums     -- かけ算部分をかけた結果
  let sbs = nub $ map fromIntegral $ sobun (fromIntegral prod)
                           -- かけ算の結果を素因数分解して重複しない數のリスト
  let isw = length sbs >= dn   -- 割算の數は因数の數を越えてゐないか
  let wcho = if ispr && isw then choose (length sbs) dn else [] 
  chdi <- if null wcho then return 0 else getRan (length wcho)
  let numds = if null wcho then sbs else map (sbs !!) (wcho!!chdi)
                                                -- 割り算に使ふ數字のリスト
  let dn' = length numds                          --最終的な割り算の個数
  let choList = if not ispr then [] else choose (pn+dn') dn'  
  let lnCho = length choList 
  pnm <- if not ispr then return 0 else getRan lnCho 
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
  return (str,res)


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
