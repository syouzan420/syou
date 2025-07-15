module Generate(genNoticeCon,genBackCon,genIntroCons
               ,genSaveData,genKamokuCons,genKamokuMonCons
               ,genIchiranCons
               ,genKGauge
               ,genConfirmCons
               ,genKanjiPreviewCons
               ,changeBColor,changeFColor
               ,changeText,changeEvent
               ) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Libs (getIndex)
import Getting (getExtStages,makeConsRec,makeBtmRec,makeSConRec
               ,makeEConRec,makeSumConsRec,stageChars,stageCharsEx
               ,getCellSize,getChiriPic,getChiriAns)
import Initialize (emCon)
import Bunsu (Bunsu(..))
import Libs (toMon)
import Define (ltQuestSrc,clearScore,mTimeLimit,qTimeLimit,expLst
              ,defGSize,defPlGPos,chPicH
              ,Pos,Size,GPos,GSize,QSource,Kmon 
              ,State(..),Con(..),Question(..),CRect(..),Gauge(..),Board(..)
              ,Bord(..),Event(..),TxType(..),Stage(..),MType(..),BMode(..)
              ,Obj(..),Role(..),DCon(..),Dir(..),Ken(..),Kan(..),San(..)
              ,Nt(..),Mdts(..),UpDown(..),SaveType(..),LSA(..))

changeBColor :: Int -> Con -> Con
changeBColor i co = co{borCol=i}

changeFColor :: Int -> Con -> Con
changeFColor i co = co{filCol=i}

changeText :: [String] -> Con -> Con
changeText txs co = co{txts=txs}

changeEvent :: Event -> Con -> Con
changeEvent ev co = co{clEv=ev}

toDataKmon :: [Kmon] -> String
toDataKmon km = intercalate "," $ concatMap (\(m,a) -> [m,a]) km

genSaveData :: SaveType -> State -> String
genSaveData sv st =
  let dts = case sv of
              ClData -> [show (clik st)]
              KData -> [toDataKmon (knjs st)]
   in "\""++intercalate "~" dts ++"\""

--テキストのみ
genTextCon :: Int -> CRect -> Int -> Int -> String -> Con
genTextCon i rec fsz txco tx =   
  emCon{conID=i,cRec=rec,txtPos=[(20,20)],txtFsz=[fsz],txtCos=[txco]
       ,txts=[tx],typs=[Normal]}

--3文字からなる下部のボタン
genBtCon :: Size -> Int -> Int -> Int -> Int -> String -> Event -> Con
genBtCon cvSz@(cW,_) i bc fc tc tx ev =   
  let hcW = cW/8*3-20
      indY = 50
      spcX = 50
      fsz = 40
      tps = [(hcW-spcX,indY),(hcW,indY),(hcW+spcX,indY)]
      tx' = map (:[]) (take 3 tx)
   in emCon{conID=i,cRec=makeBtmRec cvSz
                   ,border=Round,borCol=bc,filCol=fc,txtPos=tps
                   ,txtFsz=[fsz,fsz,fsz],txtCos=[tc,tc,tc]
                   ,txts=tx',typs=[Normal,Normal,Normal],clEv=ev} 

-- hsc (height scale: default: 7, thin: 5)
genUDCons :: Size -> Double -> [Int] -> [Int] -> Bool -> [String]
                  -> [Event] -> Maybe Event -> [Con]
genUDCons (cW,cH) hsc flco txco txdir txts clev bcev =
  let mgnX = cW/8; mgnY =cH/15
      conW = cW*3/4; conH = cH*hsc/18
      fsz = 40
      fsD = fromIntegral fsz
      lng = length flco
      txpos = if txdir then (fsD*2,fsD*2) else (cW*2/3-mgnX-fsD/2,fsD)
      stRecs = map ((\i -> CRect mgnX (mgnY*(i+1)+conH*i) conW conH).fromIntegral)
                                                                         [0..(lng-1)]
      bcon = case bcev of
                Just bev -> [genBackCon (cW,cH) lng bev]
                Nothing -> []
      ncons = zipWith (\i rec -> emCon{conID=i,cRec=rec,border=Round,borCol=1
                ,filCol=flco!!i ,txtPos=[txpos],txtFsz=[fsz]
                ,txtCos=[txco!!i],txtDir=[txdir],txts=[txts!!i]
                ,typs=[Normal],clEv=clev!!i}) [0..(lng-1)] stRecs
   in ncons++bcon

genUpDownCons :: Size -> Double -> UpDown -> Int -> Int -> Int -> Mdts -> [Con]
genUpDownCons (cW,cH) mgnY ud ind lv qn mdts =
  let mgnX = cW/15
      conW = cW/6; conH = cH/6
      fsz = 30
      fsD = fromIntegral fsz
      stRecs = [CRect mgnX mgnY conW conH
               ,CRect (mgnX+conW) mgnY conW conH
               ,CRect (mgnX+conW*2) mgnY conW conH
               ,CRect (mgnX+conW*3) mgnY conW conH
               ]
      title
          | ud==Level = "レベル"
          | ud==QNum = "問題數"
          | otherwise = ""
      showItem
          | ud==Level = show lv 
          | ud==QNum = show qn
          | otherwise = ""
      udEvents
          | ud==Level = [Kamoku (lv+1) qn mdts,Kamoku (lv-1) qn mdts]
          | ud==QNum = [Kamoku lv (qn+1) mdts,Kamoku lv (qn-1) mdts]
          | otherwise = [NoEvent,NoEvent]
      bds = [Round,Round,NoBord,NoBord]
      txs = [" ↑"," ↓",title,showItem]
      txcol n = if n<2 then 7 else 1
      evs = udEvents++[NoEvent,NoEvent]
   in zipWith (\i rec -> emCon{conID=i+ind,cRec=rec,border=bds!!i,borCol=0
                          ,filCol=1
                          ,txtPos=[(fsD/2,fsD)],txtFsz=[fsz]
                          ,txtCos=[txcol i],txts=[txs!!i],typs=[Normal],clEv=evs!!i})
                                                                [0..3] stRecs
genLevelCons :: Size -> Int -> Int -> Mdts -> [Con]
genLevelCons cvSz@(_,cH)  = genUpDownCons cvSz (cH*4/7) Level 9

genQNumCons :: Size -> Int -> Int -> Mdts -> [Con]
genQNumCons cvSz@(_,cH) = genUpDownCons cvSz (cH*3/4) QNum 3  

genQNumKCons :: Size -> Int -> Int -> Mdts -> [Con]
genQNumKCons cvSz@(_,cH) = genUpDownCons cvSz (cH*3/4) QNum 4  

genMakeMKCon :: Size -> Int -> Int -> Mdts -> Con
genMakeMKCon cvSz@(_,cH) = genMakeMCon cvSz (cH*3/4) 8 

genMakeMSCon :: Size -> Int -> Int -> Mdts -> Con
genMakeMSCon cvSz@(_,cH) = genMakeMCon cvSz (cH*3/5) 7 

genMakeMCon :: Size -> Double -> Int -> Int -> Int -> Mdts -> Con
genMakeMCon (cW,cH) mgnY ind lv qn mdts =
  let mgnX = cW/15
      conW = cW/6; conH = cH/6
      fsz = 30
      fsD = fromIntegral fsz
      stRec = CRect (mgnX+conW*4) mgnY conW conH
      empty = case mdts of Mch _ -> Mch []; Mkn _ km -> Mkn [] km; Msn _ -> Msn []
      nt = Nt (ind+1) 4 "問題をつくるよ" (Kamoku lv qn empty)
   in emCon{conID=ind,cRec=stRec,border=Round,borCol=0
           ,filCol=2,txtPos=[(fsD/2,fsD)],txtFsz=[fsz]
           ,txtCos=[7],txts=["つくる"],typs=[Normal],clEv=Notice nt}

genNextCon :: Size -> Int -> Event -> Con
genNextCon cvSz idNum = genBtCon cvSz idNum 1 6 5 "つぎへ"

genKanjiPreviewCons :: Size -> String -> [Con]
genKanjiPreviewCons cvSz@(cW,_) str =
  let rec = genKamokuMonRect cvSz
      fsz = 30; fsD = fromIntegral fsz
      (mon,ans) = toMon str
      tx = "問題は\r"++mon++"\r答へは\r"++ans++"\rとなります"
      ncon = emCon{conID=0,cRec=rec,border=NoBord
                  ,txtPos=[(cW*2/3,fsD)],txtFsz=[fsz]
                  ,txtCos=[7],txts=[tx],typs=[Normal],clEv=NoEvent}
      okcon = genBtCon cvSz 1 1 6 5 "きめる" (AddKmon (mon,ans))
      bkcon = genBackCon cvSz 2 Intro 
   in [ncon,okcon,bkcon]

genKamokuMonRect :: Size -> CRect
genKamokuMonRect (cW,cH) =
  let mgnX = cW/8; mgnY =cH/15
      conW = cW*9/10; conH = cH*3/4
   in CRect mgnX mgnY conW conH

genKamokuMonCons :: Size -> Bool -> Int -> [Int] -> Mdts -> [Con]
genKamokuMonCons cvSz@(cW,cH) isa qn clK mdts = 
  let (ia,lv,tx,fsz,pNum,mPos) = case mdts of  --ia: 全體の中でのインデックス
        Mkn kns nKmns -> let (Kan _ (mon,ans)) = kns!!qn
                             tx' = if isa then ans else mon 
                             ia' = getIndex (mon,ans) nKmns 
                          in (ia',0,show (qn+1) ++ "\r\r" ++ tx',40,0,(0,0))
        Mch kns -> let tken = kns!!qn
                       qnStr = show (qn+1)
                       (pNum',mPos') = getChiriPic tken 
                       ans = getChiriAns tken
                    in (0,0,if isa then qnStr++"\r\r"++ans else qnStr,40,pNum',mPos')
        Msn sns -> let San lv' (mon,res@(B _ rS)) = sns!!qn
                 --      resStr = if rS<0 then "－"++tail (show res) else show res
                       ans = mon ++ "\r\r = " ++ show res 
                       tx' = show (qn+1) ++ ".\r\r\r" ++ if isa then ans else mon 
                       fsz' 
                         | lv<2 = 28
                         | lv<4 = 25
                         | lv<6 || lv==9 = 22
                         | lv==8 = 30
                         | otherwise = 19
                    in (0,lv',tx',fsz',0,(0,0))
   in genKamokuMonAllCons ia lv tx fsz pNum mPos cvSz isa qn clK mdts 

genKamokuMonAllCons :: Int -> Int -> String -> Int -> Int -> Pos 
                              -> Size -> Bool -> Int -> [Int] -> Mdts -> [Con]
genKamokuMonAllCons ia lv tx fsz pNum mPos cvSz@(cW,_) isa qn clK mdts =
  let fsD = fromIntegral fsz
      rec = genKamokuMonRect cvSz 
      nqn = qn+1
      (mdLen,isChi,isSan) = case mdts of
        Mkn kns _ -> (length kns,False,False) 
        Mch kns -> (length kns,True,False)
        Msn sns -> (length sns,False,True)
      ev = if mdLen == nqn then Kamoku lv nqn mdts else KamokuMon isa nqn mdts 
      bev = if qn==0 then Kamoku lv mdLen mdts else KamokuMon isa (qn-1) mdts 
      baseCon = emCon{conID=0,cRec=rec,border=NoBord
                          ,txtPos=[(cW*2/3,fsD)]
                          ,txtFsz=[fsz]
                          ,txtCos=[7],txts=[tx],typs=[Normal]
                          ,clEv=NoEvent}
      ncon
         | isChi = baseCon {picPos=[(0,0)],ponPos=[mPos],ponCos=[3]
                           ,picSize=[(300,300)],picNums=[pNum]}
         | isSan = baseCon {alpDir=[True],txtCos=[1]}
         | otherwise = baseCon
      btcon = genNextCon cvSz 1 ev
      bkcon = genBackCon cvSz 2 bev 
      chcon = [genCheckCon cvSz 3 ia 
                  | isa && not isChi && not isSan && ia `notElem` clK]
   in [ncon,btcon,bkcon]++chcon

genCheckCon :: Size -> Int -> Int -> Con
genCheckCon cvSz@(cW,cH) i ia =
  let mgnX = cW/8; mgnY = cH/2
      fsz = 25 
      fsD = fromIntegral fsz
      ev = Check ia 
   in emCon{conID=i,cRec=CRect mgnX mgnY mgnX (mgnX*2)
           ,border=Round, borCol = 7, filCol = 2 
           ,txtPos=[(fsD/4,fsD)],txtFsz=[fsz],txtCos=[7]
           ,txts = ["覺へた"], typs=[Normal], clEv=ev}

genIchiranCons :: Size -> Int -> [Int] -> Int -> Mdts -> [Con]
genIchiranCons cvSz@(cW,cH) pg cls qn mdts@(Mkn _ nKmns) =
  let mgnX = cW/15; mgnY= cH/19 
      conW = mgnX*2; conH = mgnY*8 
      ktxListPre = drop (pg*10) (zip (map fst nKmns) [0..])
      lngListPre = length ktxListPre
      ktxList = take 10 ktxListPre 
      txsInd = map (\(mon,ia)->(show (ia+1)++"\r"
                      ++(if ia `elem` cls then " " else "✔")
                      ++mon,ia)) ktxList
      mgnXYs = map (\i -> (mgnX*(5-i)+conW*(4-i),mgnY)) [0..4] ++ 
               map (\i -> (mgnX*(5-i)+conW*(4-i),mgnY+conH)) [0..4]
      zipping = zip (zip mgnXYs txsInd) [0..9] 
      ev = if lngListPre <= 10 then Kamoku 0 qn mdts 
                               else Ichiran Nothing (pg+1) qn mdts
      bev = if pg==0 then Kamoku 0 qn mdts else Ichiran Nothing (pg-1) qn mdts
      btcon = genMiniNextCon cvSz 10 ev
      bkcon = genBackCon cvSz 11 bev 
      cons = map (\(((mx,my),(tx,ia)),i) 
                -> genIchiranKCon (CRect mx my conW conH) i tx ia pg qn mdts) zipping 
   in cons++[btcon,bkcon]

genIchiranKCon :: CRect -> Int -> String -> Int -> Int -> Int -> Mdts -> Con 
genIchiranKCon rec ind tx ia pg qn mdts =
  let fsz = 25; fsD = fromIntegral fsz
      ev = Ichiran (Just ia) pg qn mdts 
   in emCon{conID=ind,cRec=rec,border=NoBord,txtPos=[(fsD*6/5,fsD)]
           ,txtFsz=[fsz],txtCos=[1],txts=[tx],typs=[Normal],clEv=ev}

genKamokuCons :: Size -> Int -> Int -> Mdts -> [Con]
genKamokuCons cvSz lv qn mdts = 
  let bcpr = [3,9]
      bcpr2 = bcpr++[8]
      tcpr = [7,1]
      tcpr2 = tcpr++[2]
      txpr = ["問題","解答"]
      txpr2 = txpr++["一覧"]
      evfn b = case mdts of
                Mkn [] _ -> NoEvent
                _ -> KamokuMon b 0 mdts
      evpr = [evfn False,evfn True]
      evpr2 = evpr++[Ichiran Nothing 0 qn mdts]
      bcev = Just Intro 
      cns1 = case mdts of
                Mkn _ _ -> genUDCons cvSz 3 bcpr2 tcpr2 True txpr2 evpr2 bcev 
                Msn _ -> genUDCons cvSz 3 bcpr tcpr True txpr evpr bcev
                _     -> genUDCons cvSz 5 bcpr tcpr False txpr evpr bcev
      cns2 = case mdts of
                Msn _ -> genQNumCons cvSz lv qn mdts ++ genLevelCons cvSz lv qn mdts  
                _     -> genQNumKCons cvSz lv qn mdts 
      cnmk = case mdts of
                Msn _ -> genMakeMSCon cvSz lv qn mdts
                _     -> genMakeMKCon cvSz lv qn mdts
   in cns1++cns2++[cnmk]

genIntroCons :: Size -> [Con]
genIntroCons cvSz@(cW,cH) =
  let bcpr = [3,9,2]
      tcpr = [7,1,7]
      txpr = ["都道府県","漢字","計算"]
      evpr = [Kamoku 0 10 (Mch []),Kamoku 0 10 (Mkn [] []),Kamoku 3 5 (Msn [])]
      bcev = Nothing
      rscCon = genResetCon cvSz (cW*22/25,cH/45) ClData 3 
      addCon = genAddDataCon cvSz (cW*22/25,cH*20/45) 4
      rskCon = genResetCon cvSz (cW/25,cH*20/45) KData 5 
      cnsv = genClearSaveCon cvSz 6 
      cons = genUDCons cvSz 4 bcpr tcpr True txpr evpr bcev
   in cons++[rscCon,addCon,rskCon,cnsv]

genNoticeCon :: Size -> Nt -> Con
genNoticeCon (cW,cH) (Nt i flco tx ev) = 
  let mgnX = cW/3; mgnY = cH/6
      fsz = 40
      fsD = fromIntegral fsz
   in emCon{conID=i,cRec=CRect mgnX mgnY (cW-mgnX*2) (cH-mgnY*2)
           ,border=Round, borCol = 5, filCol = flco 
           ,txtPos=[(cW/2-mgnX-fsD/2,fsD*3)],txtFsz=[fsz],txtCos=[3]
           ,txts = [tx], typs=[Normal], clEv=ev}

genBackCon :: Size -> Int -> Event -> Con
genBackCon (cW,cH) i ev =
  let mgnX = cW/25; mgnY = cH/45 
      conW = cW/12
      fsz = 32
      fsD = fromIntegral fsz
   in emCon{conID=i,cRec=CRect mgnX mgnY conW conW,border=Circle
           ,borCol=0,filCol=7,txtPos=[(0,fsD/3*2)],txtFsz=[fsz],txtCos=[1]
           ,txts=["←"],typs=[Normal],clEv=ev}

genMiniNextCon :: Size -> Int -> Event -> Con
genMiniNextCon cvSz@(cW,cH) i ev = 
  let conW = cW/12 
   in (genBackCon cvSz i ev){cRec=CRect (cW-conW*3/2) (cH-conW*3/2) conW conW ,txts=["→"]}

genConfirmCons :: Size -> String -> LSA -> [Con] -> [Con]
genConfirmCons cvSz@(cW,cH) tx lsaSt cns =
  let mgnX = cW/3 
      conW = cW-mgnX*2
      fsz = 30
      fsD = fromIntegral fsz
      ncons = map (changeEvent NoEvent) cns 
      lng = length cns
      srCon = genNoticeCon cvSz (Nt lng 3 tx NoEvent)
      srCon' = srCon{txtPos=[(conW-fsD*2,fsD)],txtFsz=[fsz],txtCos=[7]}
      nCon = genNoticeCon cvSz (Nt (lng+1) 7 "いいえ" Intro)
      nCon' = nCon{cRec=CRect (cW/20) (cH/3*2) conW (cH/4)
                  ,txtPos=[(conW-80,40)],txtCos=[0]} 
      yCon = genNoticeCon cvSz (Nt (lng+2) 7 "はい" (Storage lsaSt)) 
      yCon' = yCon{cRec=CRect (cW/3*2-cW/20) (cH/3*2) conW (cH/4)
                  ,txtPos=[(conW-80,80)]}
   in ncons++[srCon',yCon',nCon']

genResetCon :: Size -> Pos -> SaveType -> Int -> Con
genResetCon cvSz@(cW,_) (mgnX,mgnY) sv i =
  let conW = cW/12
      fsD = 32
      rec = CRect mgnX mgnY conW conW
      txp = [(fsD/5,fsD/4*3)]
      bcon = genBackCon cvSz i (IsReset sv)
   in bcon{cRec=rec,txts=["×"],txtPos=txp}

genAddDataCon :: Size -> Pos -> Int -> Con
genAddDataCon cvSz@(cW,_) (mgnX,mgnY) i =
  let conW = cW/12
      fsD = 32
      rec = CRect mgnX mgnY conW conW
      txp = [(fsD/5,fsD/4*3)]
      bcon = genBackCon cvSz i AddData 
   in bcon{cRec=rec,txts=["+"],txtPos=txp}

genClearSaveCon :: Size -> Int -> Con
genClearSaveCon cvSz@(cW,cH) i =
  let conW = cW/12
      mgnX = cW-conW*3/2; mgnY = cH-conW*3/2
      fsD = 32
      rec = CRect mgnX mgnY conW conW
      txp = [(fsD/5,fsD/4*3)]
      bcon = genBackCon cvSz i (IsSave ClData) 
   in bcon {cRec=rec,txts=["S"],txtPos=txp}

genKGauge :: Size -> Int -> [Kmon] -> Gauge
genKGauge (cW,cH) cln nKmns =
  let mgnX = cW*3/8; mgnY = cH/25
      gW = cW/3; gH = 10;
      klng = length nKmns 
      par = floor (fromIntegral cln/fromIntegral klng*100)
   in Gauge "達成度" (mgnX,mgnY) (gW,gH) 100 par "100%"
