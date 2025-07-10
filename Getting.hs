module Getting (getExtStages,findCon,getConID,stageChars,stageCharsEx
               ,makeConsRec,makeBtmRec,makeSConRec,makeEConRec,makeSumConsRec
               ,getBoardEv,getScore,loadState,loadState2,getOstInd,getCellSize
               ,getPlPos,makeBKos,makeBNes,getPlNDir,getNextPos,getChiriPic
               ,getChiriAns) where

import qualified Data.Map as M
import Data.Array ((!))
import Libs (sepByChar)
import Define (Pos,Size,GSize,Kmon,CRect(..),Con(..),State(..)
              ,Board(..),BMode(..),BEvent(..),BKo(..),BNe(..),MType(..)
              ,DCon(..),Obj(..),Dir(..),Role(..),PEvent(..),Ken(..)
              ,extStages,initBKoW,initBKoH,initBNeW,initBNeH,ltQuestSrc,ostIndArr
              ,kenPosList,kenList,kenchoList)

loadState :: String -> State -> State
loadState "" st = st
loadState str st =
  let dt = if head str=='\"' then tail$init str else str
      dts = sepByChar '~' dt
      clearKData = read (head dts) :: [Int]
   in st{clik=clearKData}

loadState2 :: String -> State -> State
loadState2 "" st = st
loadState2 str st =
  let dt = if head str=='\"' then tail$init str else str
      dts = sepByChar '~' dt
      kanjiData = head dts
   in st{knjs=toKmons kanjiData}

toKmons :: String -> [Kmon]
toKmons str = let dts = sepByChar ',' str in toKmons' dts
    
toKmons' :: [String] -> [Kmon]
toKmons' [] = []
toKmons' [_] = []
toKmons' (m:k:xs) = (m,k):toKmons' xs

getChiriPic :: Ken -> (Int,Pos)
getChiriPic (Ken i n) = (i,(kenPosList!!i)!!n) 

getChiriAns :: Ken -> String
getChiriAns (Ken i n) = let ken = (kenList!!i)!!n 
                            kencho = (kenchoList!!i)!!n
                         in ken++"\r"++kencho

getScore :: MType -> Int -> Int -> Int
getScore Mi lv ms = lv-ms*2
getScore Qu lv _  = lv 
getScore _ _ _ = 0

stageChars :: Int -> (Int,[(Int,Char)])
stageChars stg =
  let dvTwo = stg `div` 2
      mdTwo = stg `mod` 2
      psi = if mdTwo==0 then dvTwo*12 else dvTwo*12+5
      tkn = if mdTwo==0 then 5 else 7
      taso = take tkn (drop psi (M.toList ltQuestSrc)) -- target associate list
   in (tkn,taso) 

stageCharsEx :: Int -> (Int,[(Int,Char)])
stageCharsEx stg
  | stg < 8 = stageChars stg
  | otherwise = let iex = stg-8
                    exInd = extStages!!iex
                 in foldl (\(k,a) s -> 
                            let (tk,ta) = stageChars s in (k+tk,a++ta))
                          (stageChars (head exInd)) (tail exInd) 

getExtStages :: [Int] -> [Int]
getExtStages clind = getExtStages' 8 clind extStages

getExtStages' :: Int -> [Int] -> [[Int]] -> [Int]
getExtStages' _ _ [] = []
getExtStages' i clind (x:xs) = 
  let bl = all (`elem` clind) x 
      next = getExtStages' (i+1) clind xs
   in if bl then i:next else next 

findCon :: Int -> [Con] -> Maybe Con
findCon _ [] = Nothing  
findCon i (co:xs) = let cid = conID co in if i==cid then Just co else findCon i xs

getConID :: Pos -> [Con] -> Int
getConID _ [] = -1
getConID mps (co:xs) = 
   if isInRec mps (cRec co) then conID co else getConID mps xs  

makeSumConsRec :: Size -> Int -> [CRect]
makeSumConsRec (cW,cH) tkn =
  let conW = cW/5; conH = cH/10
      mgnX = cW/9*4; mgnY = cH/60
      spY = cH/60 
   in map (\i -> CRect mgnX (mgnY+(conH+spY)*fromIntegral i) conW conH) [0..tkn]

makeEConRec :: Size -> Int -> CRect
makeEConRec (cW,cH) ei =
  let i = fromIntegral (ei - 8)
      conW = cW/5 
      mgnX = cW/15; mgY = cH/30
      spX = cW/7; spY = cH/8; spY0 = cH/12
      gapX = conW+spX; gapY = conW+spY; gapY0 = conW+spY0
      (px,py)
        | i==0 = (mgnX+gapX,mgY)
        | i <4 = (mgnX+gapX*(i-1),mgY+gapY0)
        | i <7 = (mgnX+gapX*(i-4),mgY+gapY0+gapY)
        | i<10 = (mgnX+gapX*(i-7),mgY+gapY0+gapY*2)
        | otherwise = (mgnX+gapX,mgY+gapY0*2+gapY*2)
   in CRect px py conW conW

makeSConRec :: Size -> Int -> CRect 
makeSConRec (cW,cH) i =
  let conW = cW/3; conH = cH/6
      mgX = cW/8; mgY = cH/12
      spX = cW/10; spY = cH/16
      yi = i `div` 2
      xi = i `mod` 2
   in CRect (mgX+(conW+spX)*fromIntegral xi) (mgY+(conH+spY)*fromIntegral yi) conW conH

makeBtmRec :: Size -> CRect
makeBtmRec (cW,cH) = let indX = cW/8; indY = cH/6
                         conW = cW-indX*2; conH = cH/8
                      in CRect indX (cH-indY) conW conH

makeConsRec :: Size -> Int -> [CRect]
makeConsRec (cW,cH) i =
  let bi = div i 2 -- the numbers of the bottom lined cons 
      biD = fromIntegral bi
      rm = mod i 2 -- the reminder (0 or 1)
      ti = bi + rm -- the numbers of the top lined cons
      tiD = fromIntegral ti
      cntX = cW/2  -- center x
      spX = cW/16 - fromIntegral (div (i-3) 2)
      spY = cW/20
      mgnX = spX*2
      mgnY = cH/10
      conW = (cW - mgnX*2 - spX*(tiD-1))/tiD
      conH = cH/5 - fromIntegral (div (i-3) 2) *3
      psX n = mgnX + (conW + spX)* fromIntegral n
      mgnBX = (cW - (conW*biD + spX*(biD-1)))/2
      psBX n = mgnBX + (conW + spX)* fromIntegral n
      tpsY = cH/4+mgnY
      bpsY = tpsY + conH + spY
      tps = map (\n -> (psX n,tpsY)) [0..(ti-1)]
      bps = map (\n -> (psBX n,bpsY)) [0..(bi-1)]
      recs = map (\(px,py) -> CRect px py conW conH) (tps ++ bps)
   in recs 

getBoardEv :: Pos -> Board -> BEvent
getBoardEv _ (Board NoB _ _ _ _) = NoBEvent
getBoardEv mps (Board Ko bps bsc _ _) = getBKoEv mps (makeBKos bps bsc) 
getBoardEv mps (Board (Ne i) bps bsc _ _) = getBNeEv mps (makeBNes i bps bsc)  
getBoardEv _ _ = NoBEvent

isInRec :: Pos -> CRect -> Bool
isInRec (x,y) (CRect rx ry rw rh) = x > rx && x < rx+rw && y > ry && y < ry+rh

getBNeEv :: Pos -> [BNe] -> BEvent
getBNeEv _ [] = NoBEvent
getBNeEv mps (BNe brec bev:xs) =
   if isInRec mps brec then bev else getBNeEv mps xs  

getBKoEv :: Pos -> [BKo] -> BEvent
getBKoEv _ [] = NoBEvent
getBKoEv mps (BKo brec bev:xs) =
   if isInRec mps brec then bev else getBKoEv mps xs  

makeBKos :: Pos -> Double -> [BKo]
makeBKos (bx,by) bsc = map (\i -> 
  let row = fromIntegral $ i `div` 3
      col = fromIntegral $ i `mod` 3
      kw = initBKoW; kh = initBKoH
      rec = CRect (bx+kw*col*bsc) (by+kh*row*bsc) (kw*bsc) (kh*bsc)
   in BKo rec (GetNe i)    ) [0..8]

makeBNes :: Int -> Pos -> Double -> [BNe]
makeBNes i (bx,by) bsc = map (\j -> 
  let rowN
        | j==0 = 0
        | j>0 && j<4 = 1
        | otherwise = 2
      colN
        | j==0 || j==2 || j==4 = 1
        | j==1 || j==5 = 0
        | otherwise = 2
      rowK = fromIntegral $ i `div` 3
      colK = fromIntegral $ i `mod` 3
      kw = initBKoW; kh = initBKoH
      nw = initBNeW; nh = initBNeH
      rec = CRect (bx+(kw*(colK-1)+nw*colN)*bsc) (by+(kh*(rowK-1)+nh*rowN)*bsc)
                                                       (nw*bsc) (nh*bsc)
   in BNe rec (GetOs i j)   ) [0..maxj]
   where maxj
          | i==4 = 6
          | i==7 = 5
          | otherwise = 4

getOstInd :: Int -> Int -> Int
getOstInd 4 5 = 47  -- wa
getOstInd 4 6 = 29  -- wo
getOstInd 7 5 = 38  -- n
getOstInd i j = ostIndArr!(j,i) 

getCellSize :: Con -> GSize -> Size
getCellSize cn (gwi,ghi) =
  let (CRect _ _ conW conH) = cRec cn
   in (conW / fromIntegral gwi, conH / fromIntegral ghi)   

getPlPos :: DCon -> Pos
getPlPos (DCon _ ob _) = 
  case ob of
    [] -> (0,0)
    (x:_) -> case role x of Pl _ -> opos x; _other -> (0,0)

getPlNDir :: Pos -> DCon -> Dir
getPlNDir (x,y) dcn = 
  let (px,py) = getPlPos dcn
      cn = cBase dcn
      gsz = gSize cn
      (cx,cy) = getCellSize cn gsz
      (pcx,pcy) = (px+cx/2,py+cy/2) -- player center positon
      (rx,ry) = (x-pcx,y-pcy) -- touch position relative to the player center
      lyx = ry >= rx; lymx = ry >= (-rx) -- above the line y=x, above the line y=-x
      ndir
        | (px,py) == (0,0) = NoDir
        | lyx && lymx = North
        | lyx && not lymx = West
        | not lyx && lymx = East
        | not lyx && not lymx = South
        | otherwise = NoDir
   in ndir

getNextPos :: CRect -> Size -> Dir -> Pos -> [Obj] -> (Pos,PEvent)
getNextPos (CRect _ _ rw rh) (cw,ch) dr (x,y) objs = 
  let sx = cw/4; sy = ch/4
      (dx,dy) = case dr of
                  South -> (0,sy)
                  North -> (0,-sy)
                  East -> (sx,0)
                  West -> (-sx,0)
                  NoDir -> (0,0)
      (tx,ty) = (x+dx,y+dy)
      isEdge = tx < 0 || tx > rw-cw || ty < 0 || ty > rh-ch
      (isBlock,pev) = getPlEvent (cw,ch) (tx,ty) objs
      nps = if isEdge || isBlock then (x,y) else (tx,ty)    
   in (nps,pev)

getPlEvent :: Size -> Pos -> [Obj] -> (Bool,PEvent)
getPlEvent _ _ [] = (False, NoPEvent)
getPlEvent (cw,ch) (tx,ty) (ob:xs) = 
  let (ox,oy) = opos ob
      tex = tx+cw; tey = ty+ch; oex = ox+cw; oey = oy+ch
      isBlock = tex > ox && tx < oex && tey > oy && ty < oey  
   in if isBlock then (isBlock,Collide ob) else getPlEvent (cw,ch) (tx,ty) xs
