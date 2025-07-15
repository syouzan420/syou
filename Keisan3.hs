module Keisan3 where

import Control.Monad (replicateM)
import Data.List (nub)
import Random (getRan,getRanList)
import Sosu (sobun)
import Bunsu (inv,yakubun,negateB,(.+),(.*),Bunsu(..))

data KZ = KZ {negateP :: !Bool -- allow negative num
             ,minNum :: !Integer
             ,maxNum :: !Integer
             } deriving (Eq,Show)
             
data EX = EX {maxY :: !Integer -- max common yakusu
             ,minBo :: !Integer  -- minimum bunbo
             ,maxBo :: !Integer} deriving (Eq,Show)

data KType = Sei !KZ | Bun !EX !KZ | Syo !KZ    deriving (Eq,Show)

data Ko = KN !Bool !KType | KP !KType !Ko | KD !Bool !KType !Ko     deriving (Eq,Show)
--K: Kazu (Bool: negative), KP: Ko * Kazu, KD: Ko `div` Kazu (Bool: divide exactly)

data Kou = A !Ko | Z !Ko !Siki deriving (Eq,Show)

data Siki = K !Kou | S !Kou !Siki | P !Kou !Siki   deriving (Eq,Show)
--K: One Kou, S: Kou and Siki, P: Kou and (Siki) (Kou may be in front or back of Siki)

makeKZ :: Int -> KZ
makeKZ 0 = KZ False 1 9
makeKZ 1 = KZ False 2 9
makeKZ 2 = KZ False 1 99
makeKZ 3 = KZ False 2 99
makeKZ 4 = KZ True 1 9
makeKZ 5 = KZ True 2 9
makeKZ 6 = KZ True 1 99
makeKZ 7 = KZ True 2 99
makeKZ 8 = KZ False 1 29

makeEX :: Int -> EX
makeEX 0 = EX 0 2 99 
makeEX 1 = EX 30 10 120
makeEX 2 = EX 0 2 30 

makeKT :: Int -> KType
makeKT 0 = Sei $ makeKZ 0
makeKT 1 = Sei $ makeKZ 1
makeKT 2 = Sei $ makeKZ 2
makeKT 3 = Sei $ makeKZ 3
makeKT 4 = Bun (makeEX 1) (makeKZ 3)
makeKT 5 = Bun (makeEX 1) (makeKZ 7)
makeKT 6 = Bun (makeEX 2) (makeKZ 8)

makeKo :: Int -> Ko
makeKo 0 = KN False $ makeKT 0 -- 5, 7 
makeKo 1 = KN False $ makeKT 2 -- 45 , 4 , 27
makeKo 2 = KP (makeKT 1) $ KN False $ makeKT 1   -- 4*6
makeKo 3 = KP (makeKT 3) $ KN False $ makeKT 3   -- 42*63
makeKo 4 = KN True $ makeKT 0    -- 3, -4
makeKo 5 = KN True $ makeKT 2   -- 36, -23
makeKo 6 = KP (makeKT 1) $ KN True $ makeKT 1    -- -2*3
makeKo 7 = KP (makeKT 3) $ KN True $ makeKT 3  -- -34*8 
makeKo 8 = KD True (makeKT 3) $ KN True $ makeKT 3  -- -68/2
makeKo 9 = KD True (makeKT 3) $ KP (makeKT 3) $ KN True $ makeKT 3 --98*5/2
makeKo 10 = KN False $ makeKT 5
makeKo 11 = KN False $ makeKT 6  

makeKou :: Int -> Kou
makeKou 20 = Z (makeKo 1) (makeSiki 3)
makeKou 21 = Z (makeKo 1) (makeSiki 4) 
makeKou n = A (makeKo n) 

makeSiki :: Int -> Siki
makeSiki 0 = let k = makeKou 1 in S k (S k (K k))
makeSiki 1 = let k = makeKou 4 in S k (S k (S k (S k (K k))))
makeSiki 2 = let k = makeKou 5 in S k (S k (S k (S k (K k))))
makeSiki 3 = let k = makeKou 5; p = makeKou 6 in S p (K k)
makeSiki 4 = let k = makeKou 5; p = makeKou 6 in S k (S k (S p (S p (K k))))
makeSiki 5 = let k = makeKou 5; p = makeKou 7 in S k (S k (S p (S p (K k))))
makeSiki 6 = let k = makeKou 5; p = makeKou 9 in S k (S p (S p (K k)))
makeSiki 7 = let k = makeKou 20; l = makeKou 9; m = makeKou 1 in S k (S l (K m))
makeSiki 8 = let k = makeKou 10 in K k
makeSiki 9 = let k = makeKou 11 in S k (K k)

siki :: Int -> IO (String,Bunsu)
siki i = showSiki (makeSiki i)

showSiki :: Siki -> IO (String,Bunsu)
showSiki s = do
  (ss,sr) <- showSiki' s 
  let tk = take 1 ss
  let str = if tk == "＋" then drop 1 ss else ss
  putStrLn str 
  return (str,sr)

showSiki' :: Siki -> IO (String,Bunsu)
showSiki' (K k) = showKou k
showSiki' (S k s) = do
  (ks,kr) <- showKou k
  (ss,sr) <- showSiki' s
  wh <- getRan 2 
  if wh==0 then return (ks++ss,kr .+ sr) else return (ss++ks,sr .+ kr)


getRanNums :: Int -> Bool -> Integer -> Integer -> IO [Integer]
getRanNums i ispn miN maN = do
  let miN' = fromIntegral miN
  let maN' = fromIntegral maN
  nums <- getRanList (maN'-miN') i
  nums2 <- getRanList maN' i
  let numRes = map ((+miN) . fromIntegral) nums
  let mods = map (`mod` 2) nums2
  let res = zipWith (\n m -> if m==1 && ispn then -n else n) numRes mods
  return res

numsNegate :: Bool -> [Integer] -> IO [Integer]
numsNegate _ [] = return []
numsNegate ispn (x:xs) = do
  neg <- getRan 2 
  let negate = ispn && neg==1
      res = if negate then -x else x
  other <- numsNegate ispn xs
  return (res:other)

showKou :: Kou -> IO (String,Bunsu)
showKou (A ko) = showKouA ko 
showKou (Z ko si) = do
  whi <- getRan 2  -- 項*式 にするか 式*項にするか二択
  let maheKou = whi==0
  (kouStr,kouRes@(B _ krS)) <- showKouA ko
  (sikiStr,sikiRes) <- showSiki si 
  let kouStr' = if krS<0 then "("++kouStr++")" else tail kouStr
  let str = if maheKou then kouStr++"×("++sikiStr++")"
                       else "＋("++sikiStr++")×"++kouStr'
  let res = kouRes .* sikiRes
  return (str,res)

showKT :: KType -> IO (String,Bunsu)
showKT (Sei (KZ ispn miN maN)) = do 
  num <- getRanNums 1 ispn miN maN >>= return . head
  return (show num,B 1 num)
showKT (Bun (EX maY miB maB) (KZ ispn miN maN)) = do
  bunsi <- getRanNums 1 ispn miN maN >>= return . head
  bunbo <- getRanNums 1 False miB maB >>= return . head
  let iy = maY > 0
  yaku <- if iy then getRanNums 1 False 2 maY >>= return . head else return 0
  let bunsiYs = 
        if iy then filter (>=miN) $ takeWhile (<=maN) (map (*yaku) [1..])
              else []
  let bunboYs =
        if iy then filter (>=miB) $ takeWhile (<=maB) (map (*yaku) [1..])
              else []
  indBunsi <- if iy then getRan (length bunsiYs) else return 0
  indBunbo <- if iy then getRan (length bunboYs) else return 0
  isng <- getRan 2
  let bunsiY = if iy then bunsiYs!!indBunsi else 0
  let bunboY = if iy then bunboYs!!indBunbo else 0
  let bunsiY' = if ispn && isng==1 then -bunsiY else bunsiY
  let bs = if iy then bunsiY' else bunsi
  let bb = if iy then bunboY else bunbo
  return (show (B bb bs),yakubun (B bb bs))
showKT (Syo (KZ ispn miN maN)) = undefined 

showKouA :: Ko -> IO (String,Bunsu)
showKouA (KN isn ktp) = do 
  (kstr,res@(B _ rS)) <- showKT ktp
  neg <- getRan 2
  let isneg = isn && neg==1  -- True なら 項自體が負になる
  let kstr' = if isneg then "－"++(if rS<0 then "("++kstr++")" else kstr)
                      else "＋"++kstr 
  return (kstr',if isneg then negateB res else res)
showKouA (KP ktp ko) = do
  (kstr,kres) <- showKT ktp
  (ostr,ores) <- showKouA ko
  let iskn = head kstr == '－'
  let ison = head ostr == '－'
  fob <- getRan 2  -- Ktype is front or back of Ko
  let rstr = if fob==0 then (if iskn then "" else "＋")++kstr++"×"++if ison then "("++ostr++")" else tail ostr
                       else ostr++"×"++if iskn then "("++kstr++")" else kstr
  let res = kres .* ores
  return (rstr,res)
showKouA (KD de ktp ko) = do
  (kstr,kres@(B _ kS)) <- showKT ktp
  (ostr,ores@(B _ oS)) <- showKouA ko
  let sbs = yakusu2 oS -- 約數のリスト
  let wcho = choose (length sbs) 1 
  chdi <- getRan (length wcho) 
  let numd = sbs !! head (wcho !! chdi)
  let strd = if kS<0 then "－("++show numd++")" else show numd 
  let (kstrE,kresE) = (strd,B 1 numd)
  let iskn = head kstr == '－'
  let rstr = ostr++"÷"++if de then kstrE else if iskn then "("++kstr++")" else kstr 
  let res = if de then ores .* inv kresE else ores .* inv kres
  putStrLn rstr
  return (rstr,res)

showP :: [Int] -> [Int] -> [Int] -> String 
showP nums numds dnums = 
  let lng = length nums + length numds - 1
      hd = show (head nums)
      ms = map (("×" ++).show) (tail nums) 
      ds = map (("÷" ++).show) numds
      dsi = zip dnums ds 
      msi = zip (filter (`notElem` dnums) [0..(lng-1)]) ms
   in hd++concatMap snd (sort (msi++dsi))

yakusu :: Integer -> [Integer]
yakusu n = let sob = sobun n 
               cs = concatMap (choose (length sob)) [1..(length sob)] 
            in nub $ map (fromIntegral . product . map (map fromIntegral sob !!)) cs 

yakusu2 :: Integer -> [Integer]
yakusu2 n = [x | x <- [2..n], n `mod` x == 0]

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
