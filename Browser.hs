{-# LANGUAGE OverloadedStrings #-}
module Browser (getCanvasInfo,chColors,cvRatio,setAudio,getRanNum
                ,tcStart,tcEnd,touchIsTrue,localStore,stringToJson,setBmps) where

import Haste(JSString)
import Haste.Events (onEvent,preventDefault,KeyEvent(..),KeyData(..))
import Haste.Graphics.Canvas(Canvas,Color(RGB),Rect(..),Bitmap,loadBitmap)
import Haste.DOM (document)
import Haste.Foreign (ffi)
import Haste.JSON(JSON,encodeJSON,decodeJSON)
import Haste.JSString(pack,unpack)
import Haste.LocalStorage(setItem,getItem,removeItem)
import Haste.Audio(mkSource,newAudio,defaultAudioSettings,AudioSettings(..),Audio)
import Define (State(swc),Switch(itc),CInfo,LSA(..),SaveType(..)
              ,imgfile,wstfile,chrfile,wstAuFile,seFile,storeName,storeName2)

getRanNum :: Int -> IO Int
getRanNum = ffi "(function(i){return (Math.floor (Math.random() * i))})"

chColors :: [Color]
chColors = [RGB 200 200 180,RGB 200 255 200,RGB 255 204 153,RGB 255 153 204
           ,RGB 153 255 255,RGB 0 128 128,RGB 255 255 100,RGB 0 0 0
           ,RGB 100 100 100,RGB 0 120 30] 
-- 0:灰色 1:薄青 2:橙色 3:ピンク 4:空色 5:背景色(青緑) 6:黄色 7:黒 8:濃灰 9:緑 


canvasW :: Canvas -> IO Double 
canvasW = ffi "(function(cv){return cv.width})"

canvasH :: Canvas -> IO Double 
canvasH = ffi "(function(cv){return cv.height})"

crecW :: Canvas -> IO Double 
crecW = ffi "(function(cv){return cv.getBoundingClientRect().width})"

crecH :: Canvas -> IO Double 
crecH = ffi "(function(cv){return cv.getBoundingClientRect().height})"

getCanvasInfo :: Canvas -> IO CInfo 
getCanvasInfo c = do
  cWidth <- canvasW c
  cHeight <- canvasH c
  rcW <- crecW c
  rcH <- crecH c
  return ((cWidth, cHeight),(rcW, rcH))

cvRatio :: CInfo -> (Double,Double)
cvRatio ((cWidth,cHeight),(rcW,rcH)) = (cWidth/rcW,cHeight/rcH)

tcStart :: State -> IO State
tcStart st = if itc (swc st) then preventDefault >> return st
                             else return st

tcEnd :: State -> IO State
tcEnd st = return st{swc=(swc st){itc=False}}

touchIsTrue :: State -> IO State
touchIsTrue st = return st{swc=(swc st){itc=True}}

localStore :: LSA -> IO String 
localStore lsa = do
  let defName sv = case sv of ClData -> storeName; KData -> storeName2
  case lsa of
    Save sv dt -> setItem (pack (defName sv)) (stringToJson dt) >> return "saved"
    Load sv -> do js <- getItem (pack (defName sv)) :: IO (Either JSString JSON)
                  return (either loadError jsonToString js)
    Remv sv -> removeItem (pack (defName sv)) >> return "removed"
    _ -> return ""

loadError :: JSString -> String
loadError js = "loadError"

jsonToString :: JSON -> String
jsonToString = unpack.encodeJSON 

stringToJson :: String -> JSON
stringToJson str = let (Right j) = (decodeJSON.pack) str in j

loadImgs :: Int -> String -> [IO Bitmap]
loadImgs (-1) str = []
loadImgs i str = loadImgs (i-1) str ++ [loadBitmap (pack (str ++ show i ++".png"))]

setBmps :: IO ([Bitmap],[Bitmap],[Bitmap])
setBmps = do
  imgs <- sequence (loadImgs 7 imgfile)
--  wsts <- sequence (loadImgs 126 wstfile)
--  chrs <- sequence (loadImgs 7 chrfile)
  return (imgs,[],[])

oneAudio :: JSString -> IO Audio
oneAudio aufile = do
  let Just adSrc = mkSource aufile
  newAudio (defaultAudioSettings{audioLooping=False,audioVolume=1}) [adSrc] 

loadAudios :: Int -> String -> [IO Audio]
loadAudios (-1) str = []
loadAudios i str = loadAudios (i-1) str
                       ++ [oneAudio (pack (str ++ show i ++ ".mp3"))]

setAudio :: IO ([Audio],[Audio])
setAudio = do 
  wstAus <- sequence (loadAudios 47 wstAuFile)
  ses <- sequence (loadAudios 3 seFile)
  return (wstAus,ses)

