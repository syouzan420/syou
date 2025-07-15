module Bunsu where

import Data.List (delete)

type Bu = Integer

type Si = Integer

data Bunsu = B !Bu !Si deriving Eq

instance Show Bunsu where
  show (B b s) = if b==1 then if s<0 then "－"++ tail (show s) else show s 
                         else "| "++show b++" "++show s++" "

infixl 6 .+

infixl 7 .*

(.+) :: Bunsu -> Bunsu -> Bunsu
(.+) = addBun

(.*) :: Bunsu -> Bunsu -> Bunsu
(.*) = mulBun

addBun :: Bunsu -> Bunsu -> Bunsu
addBun (B b1 s1) (B b2 s2) = let lcmBS = lcm b1 b2 
                                 p1 = (lcmBS `div` b1)*s1
                                 p2 = (lcmBS `div` b2)*s2
                                 res = B lcmBS (p1+p2)
                              in yakubun res

mulBun :: Bunsu -> Bunsu -> Bunsu
mulBun (B b1 s1) (B b2 s2) = let res = B (b1*b2) (s1*s2)
                              in yakubun res

yakubun :: Bunsu -> Bunsu
yakubun (B b s) = let gcdBS = gcd b s 
                   in B (b `div` gcdBS) (s `div` gcdBS) 

inv :: Bunsu -> Bunsu
inv (B b s) = if b<0 then B (-s) (-b) else B s b

negateB :: Bunsu -> Bunsu
negateB  (B b s) = B (-b) s

samePart :: [Integer] -> [Integer] -> [Integer]
samePart [] _ = []
samePart _ [] = []
samePart (x:xs) ys = if x `elem` ys then x:samePart xs (delete x ys)
                                    else samePart xs ys

