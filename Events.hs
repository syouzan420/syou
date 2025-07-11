module Events(execEvent,execEventIO,evBoard,evIntro,evNotice,evKamoku,evKamokuMon) where

import Control.Monad (when,void,replicateM)
import KanjiM (kanmons)
import Getting (getScore,getOstInd)
import Generate (genNoticeCon
                ,genBackCon,genIntroCons
                ,genSaveData
                ,genKamokuCons,genKamokuMonCons
                ,genIchiranCons
                ,genConfirmCons
                ,genKanjiPreviewCons
                ,genKGauge
                )
import Random (getRanList)
import Keisan2 (siki)
import Browser (localStore,jsprompt)
import Initialize (testCon,initBoard)
import Define (mTimeLimit,clearScore,storeName
              ,Size,Kmon
              ,State(..),Event(..),Stage(..),Question(..),Con(..),MType(..)
              ,CRect(..),Score(..),Switch(..),TxType(..),LSA(..),BEvent(..)
              ,Board(..),BMode(..),Sound(..),Ken(..),Kan(..),San(..)
              ,Nt(..),Mdts(..),SaveType(..))

execEventIO :: Size -> Int -> Int -> Event -> State -> IO State
execEventIO cvSz cid conNum ev st = case ev of   
   Kamoku lv qn mdts -> evKamoku cvSz lv qn mdts st
   AddData -> evAddData cvSz st
   _ -> return $ execEvent cvSz cid conNum ev st

execEvent :: Size -> Int -> Int -> Event -> State -> State
execEvent cvSz cid conNum ev st = case ev of
   Intro -> evIntro cvSz st
   Notice nt -> evNotice cvSz nt st
   Check qn -> evCheck qn st
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
      newClearK = case mbia of
        Nothing -> clearK
        Just ia -> if ia `elem` clearK then filter (/=ia) clearK else ia:clearK
   in st{cons=genIchiranCons cvSz pg newClearK qn mdts,clik=newClearK} 

evCheck :: Int -> State -> State
evCheck ia st = let ncon =init (cons st) in st{cons=ncon,clik=ia:clik st}

evKamokuMon :: Size -> Bool -> Int -> Mdts -> State -> State
evKamokuMon cvSz isa qn mdts st =
                 st{cons=genKamokuMonCons cvSz isa qn (clik st) mdts}

evKamoku :: Size -> Int -> Int -> Mdts -> State -> IO State
evKamoku cvSz _ qn (Mkn kns _) st = do 
  let clearK = clik st
  let nKmns = kanmons ++ knjs st
  let kanmonsC = map fst $ filter (\(_,i)-> i `notElem` clearK) (zip nKmns [0..]) 
  let lngMon = length kanmonsC
  let qn'
        | null kanmonsC = 0
        | qn<1 = 1
        | qn>lngMon = lngMon 
        | otherwise = qn
  nkns <- if null kns then getRanList lngMon qn' >>= return . map (toKan kanmonsC)
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
        | lv>7 = 7
        | otherwise = lv
  let qn'
        | qn<1 = 1
        | qn>50 = 50
        | otherwise = qn
  nsns <- if null sns then replicateM qn (siki lv') >>= return . map (San lv') 
                      else return sns 
  let ncos = genKamokuCons cvSz lv' qn' (Msn nsns)
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

evConfirm :: Size -> LSA -> State -> State
evConfirm cvSz lsaSt st = 
  let tx = case lsaSt of
              Remv ClData ->  "進：しん：捗：ちょく：データをクリアする？"
              Remv KData -> "漢字データをクリアする？"
              Save ClData _ -> "進：しん：捗：ちょく：をセーブする？"
              Save KData _ -> "漢字データをセーブする？"
   in st{cons=genConfirmCons cvSz tx lsaSt (cons st)}

