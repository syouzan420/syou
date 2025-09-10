module Events(execEvent,execEventIO,evBoard,evIntro,evNotice,evKamoku,evKamokuMon) where

import Control.Monad (when,void,replicateM)
import KanjiM (kanmons)
import Getting (getScore,getOstInd)
import Generate (genNoticeCon
                ,genBackCon,genIntroCons,genIntro2Cons
                ,genSaveData
                ,genKamokuCons,genKamokuMonCons
                ,genIchiranCons
                ,genConfirmCons
                ,genKanjiPreviewCons
                ,genKGauge
                )
import Random (getRan,getRanList)
import Libs (repList,getIndex)
import Keisan3 (siki)
import Zukei (sankaku)
import Browser (localStore,jsprompt)
import Initialize (testCon,initBoard)
import Define (mTimeLimit,clearScore,storeName
              ,Size,Kmon
              ,State(..),Event(..),Stage(..),Question(..),Con(..),MType(..)
              ,CRect(..),Score(..),Switch(..),TxType(..),LSA(..),BEvent(..)
              ,Board(..),BMode(..),Sound(..),Ken(..),Kan(..),San(..),Zuk(..)
              ,Nt(..),Mdts(..),SaveType(..))

execEventIO :: Size -> Int -> Int -> Event -> State -> IO State
execEventIO cvSz cid conNum ev st = case ev of   
   Kamoku lv qn mdts -> evKamoku cvSz lv qn mdts st
   AddData -> evAddData cvSz st
   _ -> return $ execEvent cvSz cid conNum ev st

execEvent :: Size -> Int -> Int -> Event -> State -> State
execEvent cvSz cid conNum ev st = case ev of
   Intro -> evIntro cvSz st
   Intro2 -> evIntro2 cvSz st
   Notice nt -> evNotice cvSz nt st
   Check qn -> evCheck qn st
   Must qn -> evMust qn st
   KamokuMon isa qn mdts -> evKamokuMon cvSz isa qn mdts st 
   Ichiran mbia pg qn mdts -> evIchiran cvSz mbia pg qn mdts st
   IsReset sv -> evConfirm cvSz (Remv sv) st
   IsSave sv -> evConfirm cvSz (Save sv (genSaveData sv st)) st 
   AddKmon km -> evAddKmon cvSz km st
   Storage lsaSt -> let (nclik,nknjs) = case lsaSt of
                                          Remv ClData -> ([],knjs st)
                                          Remv KData  -> (clik st,[])
                                          Save _ _    -> (clik st,knjs st)
                     in evIntro cvSz st{clik=nclik,knjs=nknjs,lsa=lsaSt}
   _ -> st

evAddKmon :: Size -> Kmon -> State -> State
evAddKmon cvSz km st = let nst = st{knjs=knjs st++[km]}
                        in evConfirm cvSz (Save KData (genSaveData KData nst)) nst

evAddData :: Size -> State -> IO State
evAddData cvSz st = do
 str <- jsprompt "問題を入力してね♪\n入力例: 鳥が<飛 と>ぶ\n入力例2: <楽 たの-しい>夏：なつ：休：やす：み" 
 let ncons = genKanjiPreviewCons cvSz str
 return st{cons=ncons}

evBoard :: Size -> Int -> Int -> BEvent -> State -> State
evBoard _ _ _ NoBEvent st = st
evBoard cvSz cid conNum bev st = 
  let boardSt@(Board _ bps bsc bi xev) = board st
      nboard = case bev of
          NoBEvent -> boardSt
          GetNe i -> Board (Ne i) bps bsc bi xev
          GetOs i j -> Board (Os (getOstInd i j)) bps bsc bi xev
      (Board nbmd _ _ nbi nxev) = nboard
   in case nbmd of 
    Os i -> do
        let st' = st{seAu=[Aoss i]}
        if i==nbi 
          then execEvent cvSz cid conNum nxev st'{board=initBoard}
          else st'{board=Board Ko bps bsc bi xev}
    _ -> st{board=nboard}

evIchiran :: Size -> Maybe Int -> Int -> Int -> Mdts -> State -> State
evIchiran cvSz mbia pg qn mdts st = 
  let clearK = clik st 
      mustK = mstk st
      newClearK = case mbia of
        Nothing -> clearK
        Just ia -> if ia `elem` clearK then filter (/=ia) clearK else
                    if ia `elem` mustK then clearK else ia:clearK
      newMustK = case mbia of 
        Nothing -> mustK
        Just ia -> if ia `elem` mustK then filter (/=ia) mustK else
                    if ia `elem` clearK then ia:mustK else mustK
   in st{cons=genIchiranCons cvSz pg newClearK newMustK qn mdts
        ,clik=newClearK,mstk=newMustK} 

evCheck :: Int -> State -> State
evCheck ia st = let ncon =init (init (cons st)) 
                    clearK = clik st
                    mustK = mstk st
                    nmstk = filter (/=ia) mustK
                    nclik = ia:clearK
                 in st{cons=ncon,clik=nclik,mstk=nmstk}

evMust :: Int -> State -> State
evMust ia st = let mustK = mstk st
                   nmstk = if ia `elem` mustK then filter (/=ia) mustK else ia:mustK
                   ncon = init (init (cons st)) 
                in st{cons=ncon,mstk=nmstk}

evKamokuMon :: Size -> Bool -> Int -> Mdts -> State -> State
evKamokuMon cvSz isa qn mdts st =
                 st{cons=genKamokuMonCons cvSz isa qn (clik st) (mstk st) mdts}

evKamoku :: Size -> Int -> Int -> Mdts -> State -> IO State
evKamoku cvSz _ qn (Mkn kns _) st = do 
  let clearK = clik st
  let mustK = mstk st
  let lngMst = length mustK
  let nKmns = kanmons ++ knjs st
  let kanmonsC = map fst $ filter (\(_,i)-> i `notElem` clearK) (zip nKmns [0..]) 
  let lngMon = length kanmonsC
  let qn'
        | null kanmonsC = 0
        | qn<1 = 1
        | qn>lngMon = lngMon 
        | otherwise = qn
  nkns <- if null kns then do
               iLst <- getRanList lngMst lngMst 
               let mustKR = map (mustK !!) iLst  -- make random mustK
               if qn' <= lngMst then do
                   let mustKR' = take qn' mustKR
                   return $ map (toKan nKmns) mustKR'
                                else do
                   lst <- getRanList lngMon (qn'-lngMst) 
                   let kns0 = map (toKan nKmns) mustKR
                       kns1 = map (toKan kanmonsC) lst
                   return (kns0++kns1)
                      else return kns 
  let ncos = genKamokuCons cvSz 0 qn' (Mkn nkns nKmns)
  return st{cons=ncos,gaus=[genKGauge cvSz (length clearK) nKmns]}
evKamoku cvSz _ qn (Mch kns) st = do 
  let qn'
        | qn<1 = 1
        | qn>46 = 46
        | otherwise = qn
  nkns <- if null kns then getRanList 47 qn' >>= return . map toKen
                      else return kns 
  let ncos = genKamokuCons cvSz 0 qn' (Mch nkns)
  return st{cons=ncos}
evKamoku cvSz lv qn (Msn sns) st = do
  let lv'
        | lv<0 = 0
        | lv>11 = 11 
        | otherwise = lv
  let qn'
        | qn<1 = 1
        | qn>50 = 50
        | otherwise = qn
  nsns <- if null sns then replicateM qn (siki lv') >>= return . map (San lv') 
                      else return sns 
  let ncos = genKamokuCons cvSz lv' qn' (Msn nsns)
  return st{cons=ncos}
evKamoku cvSz lv qn (Mzu zks) st = do
  let lv'
        | lv<0 = 0
        | lv>2 = 2 
        | otherwise = lv
  let qn'
        | qn<1 = 1
        | qn>50 = 50
        | otherwise = qn
  nzks <- if null zks then replicateM qn (sankaku lv') >>= return . map (Zuk lv') 
                      else return zks 
  let ncos = genKamokuCons cvSz lv' qn' (Mzu nzks)
  return st{cons=ncos}

toKan :: [Kmon] -> Int -> Kan
toKan kmn i = Kan 0 (kmn!!i)

toKen :: Int -> Ken
toKen i 
  | i==0 = Ken 0 0              -- Hokkaido
  | i>0 && i<7 = Ken 1 (i-1)    -- Tohoku
  | i>6 && i<14 = Ken 2 (i-7)   -- Kanto
  | i>13 && i<23 = Ken 3 (i-14) -- Chubu
  | i>22 && i<30 = Ken 4 (i-23) -- Kinki
  | i>29 && i<35 = Ken 5 (i-30) -- Chugoku
  | i>34 && i<39 = Ken 6 (i-35) -- Shikoku
  | i>38 && i<47 = Ken 7 (i-39) -- Kyusyu
  | otherwise = Ken (-1) 0

evNotice :: Size -> Nt -> State -> State
evNotice cvSz nt st = st{cons=cons st++[genNoticeCon cvSz nt]}

evIntro :: Size -> State -> State
evIntro cvSz st = st{cons=genIntroCons cvSz,dcon=Nothing,gaus=[]} 

evIntro2 :: Size -> State -> State
evIntro2 cvSz st = st{cons=genIntro2Cons cvSz,dcon=Nothing,gaus=[]} 

evConfirm :: Size -> LSA -> State -> State
evConfirm cvSz lsaSt st = 
  let tx = case lsaSt of
              Remv ClData ->  "進：しん：捗：ちょく：データをクリアする？"
              Remv KData -> "漢字データをクリアする？"
              Save ClData _ -> "進：しん：捗：ちょく：をセーブする？"
              Save KData _ -> "漢字データをセーブする？"
   in st{cons=genConfirmCons cvSz tx lsaSt (cons st)}

