module Zukei where

import Control.Monad (replicateM)
import Data.List (delete,nub,foldl')
import Random (getRan)
import Define (Rat3(..),TPos(..))

--data Rat3 = Rat3 !Int !Int !Int deriving (Eq,Show)

--data TPos = TPos !Pos !Pos !Pos deriving (Eq,Show)

data Tri = Tri !Double !Double !Double deriving (Eq,Show)

type Pos = (Double,Double)

type Scale = Double

type FracNum = Int

type Level = Int

type Frac = (Int,Int)


toTPos :: Tri -> TPos
toTPos (Tri a b c) = let bcos = (a^2+b^2-c^2)/(2*a)
                         bsin = sqrt (b^2 - bcos^2)
                      in TPos (0,0) (a,0) (a-bcos,bsin)

getRat3 :: IO Rat3
getRat3 = do
  a <- getRan 10 >>= return . (+1)
  let mb = if a==1 then a+2 else a+5
  let bs = filter (\i -> i /= a && gcd a i == 1) [2..mb]
  bi <- if length bs < 2 then return 0 else getRan (length bs - 1)
  let b = if null bs then a else bs!!bi
  let cs = filter (\i -> (gcd a i == 1 || gcd b i == 1) && istri a b i) [2..mb]
  ci <- if length cs < 2 then return 0 else getRan (length cs - 1)
  let c = if null cs then b else cs!!ci
  return (Rat3 a b c)

getFracs :: Level -> IO [Frac]
getFracs 0 = do
  (c,d) <- getFrac 9 
  let frList = [(a,b)| a <- [1..30], b<- [(a+1)..(a+5)]
                     , b `mod` (c+d)==0, gcd a b == 1]
  frInd <- if null frList then return 0 else getRan (length frList - 1)
  aa <- getRan 15
  let (a',b') = if null frList then (aa+1, c+d) else frList!!frInd
  return [(a',b'),(c,d)]
getFracs 1 = replicateM 2 (getFrac 9)
getFracs 2 = replicateM 2 (getFrac 15)
getFracs _ = replicateM 2 (getFrac 20)

getFrac :: Int -> IO Frac 
getFrac maxInt = do
  let frList = [(a,b)| a <- [1..maxInt], b <- [(a+1)..(a+5)], gcd a b == 1]
  frInd <- getRan (length frList - 1)
  return (frList!!frInd)

istri :: Int -> Int -> Int -> Bool
istri a b c = let mx = maximum [a,b,c] 
                  ots = delete mx [a,b,c]
               in mx < sum ots 

toTri :: Scale -> Rat3 -> Tri
toTri sc (Rat3 a b c) = let da = fromIntegral a
                            db = fromIntegral b
                            dc = fromIntegral c
                         in Tri (sc*da) (sc*db) (sc*dc)

rotate :: TPos -> Double -> TPos
rotate (TPos ab (c,d) (e,f)) rd =
  let c' = c*cos rd - d*sin rd
      d' = c*sin rd + d*cos rd
      e' = e*cos rd - f*sin rd
      f' = e*sin rd + f*cos rd
   in TPos ab (c',d') (e',f')

areaRatio :: [Frac] -> [Int]
areaRatio [] = []
areaRatio xs@((x,y):_) = let frs = fracToArea (x,1) xs
                             nfrs = map fraYaku frs
                             bbs = nub $ map snd nfrs
                             blcm = lcms bbs
                             b1 = map (frTimes blcm) nfrs
                             ar = map fst b1 
                             gc = gcds ar
                          in map (`div` gc) ar 

fracToArea :: Frac -> [Frac] -> [Frac]
fracToArea i [] = [fraYaku i]
fracToArea (x,y) ((a,b):xs) = fraYaku (x*a,y*(a+b)):fracToArea (x*b,y*(a+b)) xs 

fraYaku :: Frac -> Frac
fraYaku (x,y) = let frGcd = gcd x y in (x `div` frGcd, y `div` frGcd)

frTimes :: Int -> Frac -> Frac
frTimes t (x,y) = fraYaku (t*x,y) 

lcms :: Integral a => [a] -> a 
lcms nms 
      | null nms = 0
      | length nms == 1 = head nms
      | otherwise = foldl' lcm (lcm (head nms) (nms!!1)) (drop 2 nms)

gcds :: Integral a => [a] -> a 
gcds nms 
      | null nms = 0
      | length nms == 1 = head nms
      | otherwise = foldl' gcd (gcd (head nms) (nms!!1)) (drop 2 nms)

sankaku :: Level -> IO (Rat3,TPos,[Frac])
sankaku lv = do
  frs <- getFracs lv
--  frs <- replicateM n getFrac
  ro <- getRan 7
  let rd = pi / fromIntegral (ro+1)
  r3@(Rat3 a b c) <- getRat3
  let tp = toTPos $ toTri 50 r3
      ntp = rotate tp rd
  return (r3,ntp,frs)

