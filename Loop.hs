{-# LANGUAGE LambdaCase #-}
module Loop (inputLoop,mouseClick,timerEvent) where

import Haste.Graphics.Canvas(Canvas,Bitmap)
import Haste.Audio(Audio,play)
import Control.Monad(unless,when,void)
import Browser(chColors,cvRatio,localStore,stringToJson)
import Output(clearScreen,drawCons,drawBoard,drawDCon,drawGauges)
import Getting (findCon,getConID,getBoardEv,getPlNDir)
import Events(execEvent,execEventIO,evIntro,evNotice,evKamoku,evKamokuMon,evBoard)
import Define(storeName,Pos,Size,CInfo
             ,State(..),Switch(..),Con(..),CRect(..),Dir(..),BMode(..)
             ,Event(..),BEvent(..),Sound(..),LSA(..),Board(..))

type Bmps = ([Bitmap],[Bitmap],[Bitmap]) -- image, wosite, characters
type Auds = ([Audio],[Audio]) -- wosite sound, sound effect

mouseClick :: Canvas -> CInfo -> Bmps -> Auds -> (Int,Int) -> State -> IO State
mouseClick c ci bmps aus (x,y) st = do
  let (rtx,rty) = cvRatio ci
      nx = fromIntegral x*rtx
      ny = fromIntegral y*rty
      consSt = cons st
      dconSt = dcon st
      boardSt = board st
      bmdSt = bMode boardSt
      ncid = if null consSt then (-1) else getConID (nx,ny) (reverse consSt)
      nbev = if bmdSt==NoB then NoBEvent else getBoardEv (nx,ny) boardSt
      npdr = case dconSt of Nothing -> NoDir; Just dc -> getPlNDir (nx,ny) dc
  inputLoop c ci bmps aus ncid nbev npdr st 

inputLoop :: Canvas -> CInfo -> Bmps -> Auds
                   -> Int -> BEvent -> Dir -> State -> IO State 
inputLoop c ci@(cvSz,_) bmps (oss,ses) cid bev pdr st = do
  let consSt = cons st
      conNum = length consSt
      mbCon = if cid==(-1) then Nothing else findCon cid consSt
      st' = evBoard cvSz cid conNum bev st
  nst <- case mbCon of
              Nothing -> return st'
              Just co -> if enable co 
                then execEventIO cvSz cid conNum (clEv co) st' 
                else return st'
  unless (st==nst) $ drawScreen c ci bmps nst
  case lsa nst of
    Save dt -> void $ localStore (Save dt) storeName
    Remv -> void $ localStore Remv storeName 
    _ -> return ()
  mapM_ (\case
    Aoss osInd -> play (oss!!osInd)
    Ases seInd -> play (ses!!seInd) 
    NoSound -> return ()) (seAu nst) 
  return nst{seAu=[],lsa=NoLSA}

drawScreen :: Canvas -> CInfo -> Bmps -> State -> IO ()
drawScreen c ci bmps@(_,wbmp,_) st = 
  clearScreen c >> drawCons c ci bmps (cons st)
                >> drawDCon c ci bmps (dcon st)
                >> drawGauges c (gaus st)
                >> drawBoard c wbmp (board st)

timerEvent :: Canvas -> CInfo -> Bmps -> State -> IO State
timerEvent c ci@(cvSz,_) bmps st = do
  let itime = ita (swc st) -- is timer active? 
      nst = st -- if you want to use timer you should set up this part
--      (Score ms tm) = score st
--      mtp = mtype st
--      lv = level st
--      ngaus = genMGauges cvSz mtp (getScore mtp lv ms) (tm+1)
--      nst = if not itime then st else st{score=Score ms (tm+1),gaus=ngaus}
  when itime $ drawScreen c ci bmps nst
  return nst

