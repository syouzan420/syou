module Zukei where

import Data.List (delete)
import Random (getRan)
import Define (Rat3(..),TPos(..))

--data Rat3 = Rat3 !Int !Int !Int deriving (Eq,Show)

data Tri = Tri !Double !Double !Double deriving (Eq,Show)

type Pos = (Double,Double)

type Scale = Double

--data TPos = TPos !Pos !Pos !Pos deriving (Eq,Show)

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

istri :: Int -> Int -> Int -> Bool
istri a b c = let mx = maximum [a,b,c] 
                  ots = delete mx [a,b,c]
               in mx < sum ots 

toTri :: Scale -> Rat3 -> Tri
toTri sc (Rat3 a b c) = let da = fromIntegral a
                            db = fromIntegral b
                            dc = fromIntegral c
                         in Tri (sc*da) (sc*db) (sc*dc)

sankaku :: IO (Rat3,TPos)
sankaku = do
  r3@(Rat3 a b c) <- getRat3
  let tp = toTri (250.0/fromIntegral (maximum [a,b,c])) r3
  return (r3,toTPos tp)
