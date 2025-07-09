module GenHotuma where

import Generate

genMission :: Int -> Int -> Int -> IO (Question,Int)
genMission g stg pai = do 
  let (tkn,taso) = stageCharsEx stg
      qs = M.fromList taso
      qs' = if pai==(-1) then qs else M.delete pai qs
          --a list which eliminates the previous answer from the stage char list 
  (an,g') <- selectData 1 g (M.toList qs')            
  let ans = head an
      nqs = M.delete (fst ans) qs'
  (ai,g'') <- getRan 4 g'
  (iqlist,ng) <- selectData 3 g'' (M.toList nqs)
  let iqlist' = insToList ai ans iqlist
      (auInd,qlist) = unzip iqlist'
  return (Question (map (:[]) qlist) auInd ai,ng) 

genLtQuest :: Int -> Int -> QSource -> IO ((Question,QSource),Int)
genLtQuest g lv qs = do
  let qn = 4 + lv `div` 8 -- question numbers
  (an,g') <- selectData 1 g (M.toList qs) 
  let ans = head an
  let nqs = M.delete (fst ans) qs 
  let aqs = M.delete (fst ans) ltQuestSrc 
  (ai,g'') <- getRan qn g' 
  (iqlist,ng) <- selectData (qn-1) g'' (M.toList aqs) 
  let iqlist' = insToList ai ans iqlist
  let (auInd,qlist) = unzip iqlist'
  return ((Question (map (:[]) qlist) auInd ai,nqs),ng)

genLCons :: Size -> Int -> Event -> [Con]
genLCons (cW,cH) oi ev = 
  let conW = cW/2; conH = cH/3 
      mgnX = cW/4; mgnY = cH/8
      spY = cH/10
      tpX = 50; tpY = 120
      fsz = 100
      lConRecs = [CRect mgnX mgnY conW conH,CRect mgnX (mgnY+conH+spY) conW conH]
      och = fromMaybe ' ' $ M.lookup oi ltQuestSrc 
   in zipWith (\(i,cn) rec -> emCon{conID=i,cRec=rec
                   ,border=Round,borCol=4,filCol=cn
                   ,txtPos=[(tpX,tpY)],txtFsz=[fsz],txtCos=[1]
                   ,txts=[[och]],typs=[if i==0 then Osite else Normal]
                   ,clEv=if i==0 then NoEvent else ev
                   ,visible=i==0,enable=i==0}) [(0,9),(1,5)] lConRecs

genLBoard :: Size -> Int -> Event -> Board
genLBoard (cW,cH) oi ev = let mgnX = cW/5; mgnY = cH*3/5; scl = 1.2
                           in Board Ko (mgnX,mgnY) scl oi ev

genCons :: Size -> Question -> [Con]
genCons cSz@(cW,cH) (Question qlist auInd ai) = 
  let qLen = length qlist
      aText = qlist!!ai
      aAudio = auInd!!ai
      hConRec = CRect (cW/8) (cH/10) (cW/3) (cH/5)
      qConRecs = makeConsRec cSz qLen
      ext = fromIntegral (div (qLen-3) 2)
      ifsz = 50
      fsz = floor (fromIntegral ifsz - ext*8)
      itpX = 40
      tpX = itpX - (fromIntegral fsz/9)*4*ext
      tpY = 60
      hCon = emCon{conID=0,cRec=hConRec,border=Round,filCol=5,txtPos=[(itpX,tpY)]
                ,txtFsz=[ifsz],txtCos=[1],txts=[aText],typs=[Normal]
                ,audio=Just aAudio,clEv=NoEvent}
      qCons = zipWith (\i rec -> hCon{conID=i+1,cRec=rec,border=Round,filCol=7
                                     ,txtPos=[(tpX,tpY)]
                                     ,txtFsz=[fsz]
                                     ,txts=[qlist!!i],typs=[Osite]
                                     ,audio=Just (auInd!!i)
                                     ,clEv=Choice i}) [0..] qConRecs
   in hCon:qCons 

genSCons :: Size -> [Int] -> [Int] -> [Con]
genSCons cvSz clind hscrs =
  let tpX = 40; tpY = 60
      fsz = 50
      fsD = fromIntegral fsz
      sConRecs = map (makeSConRec cvSz) [0..7]
      lList = "あいふへもをすし"
      isclear n = n `elem` clind
      makeFlCol n = if isclear n then 9 else 7
      txps n = if isclear n then [(tpX,tpY),(tpX-fsD/2,tpY)] else [(tpX,tpY)]
      txfs n = if isclear n then [fsz,fsz `div` 2] else [fsz]
      txco n = if isclear n then [1,1] else [1]
      txs n = if isclear n then [[lList!!n],show (hscrs!!n)] else [[lList!!n]]
      typ n = if isclear n then [Osite,Normal] else [Osite]
      sCons = zipWith (\i rec -> emCon{conID=i,cRec=rec,border=Round
                              ,filCol=makeFlCol i
                              ,txtPos=txps i,txtFsz=txfs i,txtCos=txco i
                              ,txts=txs i,typs=typ i
                              ,clEv=Learn i 0}) [0..] sConRecs
      exInds = getExtStages clind
      eConRecs = map (makeEConRec cvSz) exInds
      txE n = if isclear n then show (hscrs!!n) else "" 
      fszEx = 20
      fsED = fromIntegral fszEx
      eCons = zipWith (\(i,es) rec -> emCon{conID=i+8,cRec=rec,border=Circle
                              ,filCol=makeFlCol es 
                              ,txtPos=[(fsED/2*3,fsED/2*3)],txtFsz=[fszEx]
                              ,txtCos=[1],txts=[txE es],typs=[Normal]
                              ,clEv=Mission es (-1) 0}) (zip [0..] exInds) eConRecs
   in sCons++eCons

genAnsCon :: Size -> Int -> Int -> Con
genAnsCon cvSz@(cW,_) conNum i = 
  let rec = makeBtmRec cvSz
      hcW = cW/8*3-20
      indY = 50
      spcX = 50
      tfsz = 50
      tps = [(hcW-spcX,indY),(hcW,indY),(hcW+spcX,indY)]
   in emCon{conID=conNum,cRec=rec,border=Round,borCol=1,filCol=6,txtPos=tps
           ,txtFsz=[tfsz,tfsz,tfsz],txtCos=[5,5,5]
           ,txts=["こ","た","ふ"]
           ,typs=[Normal,Normal,Normal]
           ,audio=Nothing
           ,clEv=Answer i}

genCorrectCons :: Size -> Stage -> [Con] -> [Con]
genCorrectCons (cW,cH) nstg cos = 
  let iCos = init cos
      niCos = map (changeEvent NoEvent) iCos
      lco = last cos
      lind = length cos - 1
      indX = cW/3; indY = cH/6
      fsz = 40
      fsD = fromIntegral fsz
      nlco = lco{cRec=CRect indX indY (cW-indX*2) (cH-indY*2)
                ,txtPos=[(cW/2-indX-fsD/2,fsD*3)]
                ,txtFsz=[fsz]
                ,txts=["せいかい！"]
                ,clEv=Quest nstg}
   in niCos++[nlco]

genWrongCons :: Stage -> Int -> Int -> [Con] -> [Con]
genWrongCons nstg i ai (hco:tlCos) = 
  let chCos = init tlCos
      tco = chCos!!i
      aco = chCos!!ai
      lco = last tlCos 
      ntco = changeBColor 3 tco
      naco = changeBColor 4 aco
      lco' = changeText ["は","づ","れ"] lco
      nlco = changeEvent (Quest nstg) lco'
      chCos' = repList i ntco chCos
      chCos'' = repList ai naco chCos'
      nchCos = map (changeEvent NoEvent) chCos''
   in hco:nchCos++[nlco]

--文字當てを始める前の確認
genSumCons :: Size -> Int -> [Con]
genSumCons cvSz@(cW,cH) stg =
  let (tkn,taso) = stageChars stg
      sumConRecs = makeSumConsRec cvSz tkn 
      tpX = 20; tpY = 40 
      fsz = 45 
      sumCons = zipWith (\(i,(k,ch)) rec -> emCon{conID=i,cRec=rec
                   ,border=Round,borCol=4,filCol=9
                   ,txtPos=[(tpX,tpY)],txtFsz=[fsz],txtCos=[1]
                   ,txts=[[ch]],typs=[Osite]
                   ,clEv=ChClick k}) (zip [0..(tkn-1)] taso) sumConRecs
      btCon = genBtCon cvSz tkn 1 6 5 "はじめ" (Mission stg (-1) 0) 
      noticeCon0 = genTextCon (tkn+1) (CRect (cW/4*3) (cH/20) (cW/4) (cH/2)) 30 1
                                            "もじを タップして かくにん してね"
                                      
      noticeCon1 = genTextCon (tkn+2) (CRect (cW/5) (cH/10) (cW/4) (cH/2)) 30 1
                                            "よければ もじあて を はじめやう"
      bCon = genBackCon cvSz (tkn+3) Study
   in sumCons++[btCon,noticeCon0,noticeCon1,bCon] 

genExpCons :: Size -> Int -> [Con]
genExpCons cvSz@(cW,cH) i =
  let mgnX = cW/8; mgnY =cH/15
      conW = cW*3/4; conH = cH*3/4
      fsz = 40
      fsD = fromIntegral fsz
      rec = CRect mgnX mgnY conW conH
      lngExp = length expLst
      mcon = emCon{conID=0,cRec=rec,border=NoBord
                          ,txtPos=[(cW*2/3,fsD)],txtFsz=[fsz]
                          ,txtCos=[1],txts=[expLst!!i],typs=[Normal]
                          ,clEv=NoEvent}
      ev = if i+1==lngExp then Intro else Explain (i+1)
      btcon = genNextCon cvSz 1 ev
      bkcon = genBackCon cvSz 2 (if i==0 then Intro else Explain (i-1))
   in [mcon,btcon,bkcon]

genStartCons :: Size -> [Con]
genStartCons cvSz = 
  let bcpr = (7,9)
      tcpr = (1,1)
      txpr = ("ヲシテを\rおぼへやう！","チャレンジ\rもんだい！")
      evpr = (Study,Quest (StgLetter 0))
      bcev = Just Intro
   in genUDCons cvSz 7 bcpr tcpr txpr evpr bcev

genMGauges :: Size -> MType -> Int -> Int -> [Gauge]
genMGauges (cW,cH) mtp sc tm =
  let mgnX = cW/8; mgnY = cH/20
      spX = cW/5
      gW = cW/4; gH = 10;
      tx0 = case mtp of Mi -> "SCORE"; Qu -> "LETTERS"; _ -> ""
      scMax = case mtp of Mi -> clearScore; Qu -> 48; _ -> 0
      tmLim = case mtp of Mi -> mTimeLimit; Qu -> qTimeLimit; _ -> 0
      gau0 = Gauge tx0 (mgnX,mgnY) (gW,gH) scMax sc 
      gau1 = Gauge "TIME" (mgnX+gW+spX,mgnY) (gW,gH) tmLim (tmLim-tm)
   in [gau0,gau1]

genObjects :: CRect -> GSize -> GPos -> String -> [Obj]
genObjects _ _ _ [] = []
genObjects rec@(CRect cx cy cw ch) gsz@(igw,igh) gps@(igx,igy) (x:xs) = 
  let gw = cw / fromIntegral igw; gh = ch / fromIntegral igh 
      nsc = (gw/chPicH,gh/chPicH)
      nop = (cx+gw*fromIntegral igx,cy+gh*fromIntegral igy)
      isBottom = igh==igy+1
      ngps = case x of
        '\r' -> (igx-1,0)
        _other -> (if isBottom then igx-1 else igx
                  ,if isBottom then 0 else igy+1)
      nobs = genObjects rec gsz ngps xs  
   in if x==' ' then nobs else Obj (Mz x) nop nsc 0:nobs

genDCon :: Size -> DCon
genDCon cvSz@(cW,cH) =  
  let mgX = cW/12; mgY = cH/20 
      conW = cW - mgX*2
      conH = conW*17/10
      baseCon = emCon{cRec = CRect mgX mgY conW conH,border = Rigid}
      (clw,clh) = getCellSize baseCon defGSize 
      plSc = (clw/chPicH,clh/chPicH)
      (pgx,pgy) = defPlGPos
      plPos = (clw * fromIntegral pgx,clh * fromIntegral pgy) 
      opl = Obj (Pl South) plPos plSc 0 
   in DCon baseCon defGSize [opl] 0



