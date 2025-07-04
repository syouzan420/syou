modue EvHotuma where

import Events

dcUpdate :: Dir -> State -> State
dcUpdate NoDir st = st
dcUpdate pdr st = 
  let mbDCon = dcon st
   in case mbDCon of
        Nothing -> st
        Just dcn@(DCon cbs gsz objs tmc) -> 
              let (Obj (Pl _) pps psc ai) = head objs 
                  oths = tail objs
                  rec = cRec cbs -- con rect
                  csz = getCellSize cbs gsz -- cell size
                  nai = if ai==0 then 1 else 0
                  (npos,pev) = getNextPos rec csz pdr pps oths
                  npl = Obj (Pl pdr) npos psc nai
                  nobjs = npl:oths
               in st{dcon=Just (DCon cbs gsz nobjs tmc)}

evMission :: Size -> Int -> Int -> Int -> State -> IO State
evMission cvSz stg i lv st = do 
  let pq = quest st -- previous question
      (pai,pan) = case pq of
        Just pq' -> (audios pq'!!aInd pq',aInd pq')
        Nothing -> (-1,0)
      correct = i==pan
      sa = [Ases 1 | not (correct || lv <= 0)]
  let Score ms tm = score st
      nms = if pai==(-1) || correct then ms else ms+1
      nscr = Score nms tm
      g = rgn st -- random number generator 
  (q,ng) <- genMission g stg pai 
  let (hco:cos) = genCons cvSz q
      ai = audios q!!aInd q 
      end = tm>=mTimeLimit
      ncos = hco:zipWith (\n co -> 
            changeEvent (if end then MEnd stg lv else Mission stg n (lv+1)) co)
                                                                       [0..] cos 
      ngaus = genMGauges cvSz Mi (getScore Mi lv nms) tm
  return st{mtype=Mi,level=lv,score=nscr,quest=Just q,cons=ncos,gaus=ngaus
           ,rgn=ng,swc=(swc st){ita=True},seAu=sa++[Aoss ai]}

evStgLt :: Size -> Int -> State -> IO State
evStgLt cvSz lv st = do  
  let g = rgn st -- random number generator 
      qs = qsrc st -- quest source
  ((q,nqs),ng) <- genLtQuest g lv qs
  let cos = genCons cvSz q
      tau = audios q!!aInd q
  return st{mtype=Qu,quest=Just q,level=lv,cons=cos,qsrc=nqs,rgn=ng
           ,swc=(swc st){ita=True},seAu=[Aoss tau]}

evStgWd :: Size -> Int -> State -> IO State
evStgWd cvSz lv st = undefined

evResetNotice :: Size -> State -> State
evResetNotice cvSz st = st{cons=genResetNoticeCons cvSz (cons st)}

evExplain :: Size -> Int -> State -> State
evExplain cvSz i st = st{cons=genExpCons cvSz i,dcon=Just (genDCon cvSz)}

evMEnd :: Size -> Int -> Int -> State -> State
evMEnd cvSz stg lv st = do 
  let Score ms _ = score st 
      scr = lv - ms*2
      hiscores = hiscs st
      phscr = hiscores!!stg  -- previous high score
      nhscr = max scr phscr
      nhiscs = repList stg nhscr hiscores 
      isClear = scr > clearScore
      cos = cons st
      ncons = map (changeEvent NoEvent) cos
      cln = length cos
      tx = if isClear then " クリア！" else "ざんねん！"
      flco = if isClear then 6 else 8
      atCo = genNoticeCon cvSz (Nt cln flco tx Study) 
      ci = cli st
      ncli = if isClear then if stg `elem` ci then ci else stg:ci
                        else ci
      sei = if isClear then 0 else 1
      nst = st{hiscs=nhiscs,cons=ncons++[atCo],cli=ncli,seAu=[Ases sei]}
      nlsa = if isClear then Save (genSaveData nst) else NoLSA 
   in nst{lsa=nlsa}

evChClick :: Int -> Int -> State -> State
evChClick i oi st = 
  let cos = cons st
      co = cos!!i
      tp = head (typs co) -- text type 
      eps = fromIntegral (head (txtFsz co)) / 4
      (psx,psy) = head (txtPos co) -- text position
      ntp = if tp==Osite then Normal else Osite
      ntps = if tp==Osite then (psx,psy+eps) else (psx,psy-eps) 
      nco = co{typs=[ntp],txtPos=[ntps]}
      ncons = repList i nco cos
   in st{cons=ncons,seAu=[Aoss oi]}

evSummary :: Size -> Int -> State -> State
evSummary cvSz stg st = st{cons=genSumCons cvSz stg} 

evLearn :: Size -> Int -> Int -> State -> State 
evLearn cvSz stg num st =  
  let stype = stg `mod` 2
      dif = stg `div` 2 * 12 + num
      maxNum = if stype==0 then 4 else 6
      oi = if stype==0 then dif else dif + 5 
      clEvent = if num==maxNum then Summary stg else Learn stg (num+1) 
      lCons = genLCons cvSz oi clEvent
      boardSt@(Board bmd _ _ _ _) = board st
      nboard = if bmd==NoB then genLBoard cvSz oi RevealL else boardSt
   in st{cons=lCons,board=nboard,seAu=[Aoss oi]}

evRevealL :: State -> State
evRevealL st = let cns = cons st
                   tcon = last cns
                   ncon = tcon{visible=True,enable=True}
                in st{cons=init cns++[ncon]}

evStudy :: Size -> State -> State
evStudy cvSz st =
  let clIndexes = cli st 
      hiScores = hiscs st
      ncos = genSCons cvSz clIndexes hiScores
      lngCos = length ncos
      bco = genBackCon cvSz lngCos Start
      rco = genScrResetCon cvSz (lngCos+1)
      exco = if null clIndexes then [bco] else [bco,rco]
   in st{score=Score 0 0,quest=Nothing,cons=ncos++exco,gaus=[]
        ,swc=(swc st){ita=False}}

evResult :: Size -> State -> State
evResult cvSz st =  
  let (Score mis tim) = score st 
      ntxt = if mis==0 then "すごい！ 全問正解！"
                       else show mis ++ "回まちがへちゃったね！"
      ntxt2 = "\rかかった時間は " ++ show tim ++ " 秒だったよ"
      ncon = testCon{txts=[ntxt++ntxt2]}
      nScr = Score 0 0
      nst = st{score=nScr,cons=[ncon]}
   in nst

evChoice :: Size -> Int -> Int -> State -> State
evChoice cvSz conNum i st =
  let mbQ = quest st
      qNum = case mbQ of
        Just (Question qlist _ _) -> length qlist
        Nothing -> 0
      (hco:chCos) = cons st 
      tco = chCos!!i
      chCos' = map (changeFColor 7 . changeBColor 0) chCos 
      ntco = (changeFColor 8 . changeBColor 4) tco
      chCos'' = repList i ntco chCos'
      nchCos = if qNum+1==conNum then chCos'' else init chCos'' 
      aco = genAnsCon cvSz (qNum+1) i
   in st{cons=hco:nchCos++[aco]}

evAns :: Size -> Int -> State -> State
evAns cvSz i st =  
  let mbQ = quest st
      (qs,au,ai) = case mbQ of
          Just (Question qs' au' ai') -> (qs',au',ai')
          Nothing -> ([],[],0)
      isCorrect = ai==i
      nst = st{swc=(swc st){ita=False}}
   in if isCorrect then evCorrect cvSz i nst else evWrong i nst

evCorrect :: Size -> Int -> State -> State
evCorrect cvSz i st =  
  let mbStg = stage st
      nstg = case mbStg of 
        Just (StgLetter i) -> StgLetter (i+1) 
        Just (StgWord i) -> StgWord (i+1)
        Nothing -> StgLetter 1
   in st{cons=genCorrectCons cvSz nstg (cons st),seAu=[Ases 0]} 

evWrong :: Int -> State -> State
evWrong i st = 
  let mbStg = stage st
      nstg = case mbStg of
        Just stg -> stg
        Nothing -> StgLetter 1 
      mbq = quest st
      ai = case mbq of
            Just (Question _ _ a) -> a
            Nothing -> 0
      (Score pmis tim) = score st  
   in st{score=Score (pmis+1) tim,cons=genWrongCons nstg i ai (cons st),seAu=[Ases 1]}

evRPG :: Size -> State -> State
evRPG cvSz st = st{cons=[],dcon=Just (genDCon cvSz)}

evStart :: Size -> State -> State
evStart cvSz st = st{mtype=NoMission,cons=genStartCons cvSz
                      ,gaus=[],swc=(swc st){ita=False}} 

