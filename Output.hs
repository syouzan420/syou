module Output(clearScreen,putChara,playAudio
             ,putText,drawCons,startGame
             ,drawGauges,drawBoard,drawDCon) where

import Haste.Graphics.Canvas(color,font,translate,rotate,line,arc,rect,circle
                            ,text,draw,scale,render,stroke,fill,lineWidth
                            ,renderOnTop
                            ,Canvas,Color(RGB),Bitmap,Point,Vector,Shape)
import Haste.Audio (play,Audio)
import Control.Monad (when,unless)
import Define (nfs,wstIndex,storeName
              ,Pos,Size,Fsize,CInfo
              ,State(..),Switch(..),Con(..),CRect(..)
              ,Bord(..),TxType(..),LSA(..),Gauge(..)
              ,Board(..),BMode(..),BKo(..),BNe(..)
              ,Obj(..),Role(..),DCon(..))
import Browser (chColors,localStore)
import Initialize (testCon)
import EAffirm (affr)
import Getting (loadState,makeBKos,makeBNes)
import Libs(getIndex)

type Bmps = ([Bitmap],[Bitmap],[Bitmap])


clearScreen :: Canvas -> IO ()
clearScreen c = render c $ text (0,0) "" 

putChara :: Canvas -> [Bitmap] -> Double -> Pos -> Int -> IO ()
putChara c chrs cvW pos ind = do  
  renderOnTop c $ translate pos $ scale (1,1) $ draw (chrs!!ind) (0,0)

playAudio :: Audio -> State -> IO State 
playAudio audio st = do
  let iAS = (ias . swc) st
  if iAS then return st else do
    play audio
    return st{swc=(swc st){ias=True}}

----------------------------

startGame :: Canvas -> CInfo -> Bmps -> State -> IO State 
startGame c ci bmps st = do
  randomMessage c ci bmps st 
  sData <- localStore Load storeName 
  return $ if sData=="loadError" then st else loadState sData st

randomMessage :: Canvas -> CInfo -> Bmps -> State -> IO ()
randomMessage c ci bmps st = do
  let randomG = rgn st
  (affText,ng) <- affr randomG
  let affCons = [testCon{txts=[affText]}]
  drawCons c ci bmps affCons 

drawRoundRect :: Canvas -> CRect -> Double -> (Color,Color) -> IO ()
drawRoundRect c (CRect x y w h) lnw (bcol,fcol) = do
  renderOnTop c $ color fcol $ fill $ roundRect (x,y) (w,h)
  renderOnTop c $ color bcol $ lineWidth lnw $ stroke $ roundRect (x,y) (w,h)

drawCircle :: Canvas -> CRect -> Double -> (Color,Color) -> IO ()
drawCircle c (CRect x y w h) lnw (bcol,fcol) = do
  let r = w/2
  renderOnTop c $ color fcol $ fill $ circle (x+r,y+r) r 
  renderOnTop c $ color bcol $ lineWidth lnw $ stroke $ circle (x+r,y+r) r 

drawKos :: Canvas -> [Bitmap] -> Board -> Int -> IO ()
drawKos c wbmp (Board _ bps bsc _ _) i = do
  let bkos = makeBKos bps bsc
      bcol = chColors!!1
      fcol = chColors!!(if i<0 then 3 else 8)
      f2col = chColors!!4
  mapM_ (\(BKo rec@(CRect rx ry _ _) _,(n,x)) -> do
              drawRoundRect c rec 2 (bcol,if n==i then f2col else fcol)
              putWst c wbmp 40 (rx+10,ry+10) x 
         ) (zip bkos (zip [0..] "ysrkxthnm")) 
  
drawNes :: Canvas -> [Bitmap] -> Board -> Int -> IO ()
drawNes c wbmp bd@(Board _ bps bsc _ _) i = do
  let bnes = makeBNes i bps bsc
      bcol = chColors!!1
      fcol = chColors!!3
  drawKos c wbmp bd i  
  mapM_ (\(BNe rec@(CRect rx ry _ _) _,x) -> do
               drawRoundRect c rec 2 (bcol,fcol)
               putWst c wbmp 36 (rx+9,ry+9) x 
        ) (zip bnes "aiouew雄")

drawBoard :: Canvas -> [Bitmap] -> Board -> IO ()
drawBoard c wbmp bd@(Board bmd _ _ _ _) =
  case bmd of
    NoB -> return ()
    Os _ -> return ()
    Ko -> drawKos c wbmp bd (-1)
    Ne i -> drawNes c wbmp bd i 

drawGauges :: Canvas -> [Gauge] -> IO ()
drawGauges _ [] = return ()
drawGauges c gausSt = mapM_ (putGauge c) gausSt  

drawCons :: Canvas -> CInfo -> Bmps -> [Con] -> IO ()
drawCons _ _ _ [] = return ()
drawCons c ((_,cvH),_) bmps consSt = mapM_ (putCon c cvH bmps) consSt

drawDCon :: Canvas -> CInfo -> Bmps -> Maybe DCon -> IO ()
drawDCon _ _ _ Nothing = return ()
drawDCon c ((_,cvH),_) bmps (Just (DCon cbs os _)) = do
  let (CRect cx cy _ _) = cRec cbs
  putCon c cvH bmps cbs
  mapM_ (putObj c (cx,cy) bmps) os

putGauge :: Canvas -> Gauge -> IO ()
putGauge c (Gauge title (gx,gy) (gw,gh) mx cu) = do
  let scol = chColors!!3 -- red
      mcol = chColors!!6 -- yellow
      lcol = chColors!!4 -- cyan
      scu 
        | cu<0 = 0
        | cu>mx = mx
        | otherwise = cu
      mxD = fromIntegral mx; cuD = fromIntegral scu
      mdc = mxD / cuD
      fcol
        | mdc > 3 = scol
        | mdc < 2 = lcol
        | otherwise = mcol
      bcol = head chColors
  putText c bcol (floor gh*2) (gx,gy) title
  unless (cu==0) $ renderOnTop c $ color fcol $ fill $ roundRect (gx,gy) (gw/mdc,gh)
  renderOnTop c $ color bcol $ lineWidth 1 $
                                       stroke $ roundRect (gx,gy) (gw,gh)
  putText c fcol (floor gh*2) (gx+gw,gy+gh) (show (max cu 0))

putObj :: Canvas -> Pos -> Bmps -> Obj -> IO ()
putObj c (conx,cony) bmps (Obj ro (ox,oy) sc ai) = do 
  let (_,_,chrs) = bmps
      bm = case ro of
            Pl _ -> chrs!!ai
            _ -> undefined
  renderOnTop c $ translate (conx+ox,conx+oy) $ scale sc $ draw bm (0,0)

putPoint :: Canvas -> Pos -> Color -> IO ()
putPoint c (x,y) col = drawCircle c (CRect x y 10 10) 1 (col,col)

putImg :: Canvas -> Pos -> Pos -> Bmps -> Int -> IO ()
putImg c (conx,cony) (x,y) (imgs,_,_) pNum = do
  renderOnTop c $ translate (conx+x,cony+y) $ scale (1,1) $ draw (imgs!!pNum) (0,0)

putCon :: Canvas -> Double -> Bmps -> Con -> IO ()
putCon c cvH bmps con = if not (visible con) then return () else do 
  let rec@(CRect cx cy cw ch) = cRec con
      bocol = borCol con
      ficol = filCol con
      txpos = txtPos con
      txfsz = txtFsz con
      txcol = txtCos con
      txd = txtDir con
      txdir = if null txd then replicate (length txpos) False else txd
      txs = txts con
      tps = typs con
      bcol = chColors!!bocol
      fcol = chColors!!ficol
      pcpos = picPos con
      popos = ponPos con
      pocos = ponCos con
      pcos = map (chColors !!) pocos
      pnums = picNums con
      (_,wbmp,_) = bmps
  case border con of
    Rigid -> renderOnTop c $ color bcol $ stroke $ rect (cx,cy) (cx+cw,cy+ch)
    Round -> drawRoundRect c rec 3 (bcol,fcol) 
    Circle -> drawCircle c rec 3 (bcol,fcol) 
    _ -> return ()
  mapM_ (\(pnum,(pcx,pcy)) -> putImg c (cx,cy) (pcx,pcy) bmps pnum)
                                  $ zip pnums pcpos
  mapM_ (\(pcol,(pox,poy)) -> putPoint c (cx+pox,cy+poy) pcol)
                                  $ zip pcos popos
  mapM_ (\((tx,(td,tp)),((tpx,tpy),(fz,col))) ->
          if td then
              putTextH c (chColors!!col) fz (tpx+cx,tpy+cy) tx
                else
              putTextV c wbmp (chColors!!col) tp fz (cw,ch) (tpx+cx,tpy+cy) tx)
                      $ zip (zip txs (zip txdir tps)) (zip txpos (zip txfsz txcol))

putTextH :: Canvas -> Color -> Fsize -> Point -> String -> IO ()
putTextH c col fz (x,y) str = do
  let strs = lines str
  mapM_ (\(tx,i) -> putText c col fz (x,y+fromIntegral fz*2*i) tx) (zip strs [0..]) 

putText :: Canvas -> Color -> Fsize -> Point -> String -> IO ()
putText c col fz (x,y) str = renderOnTop c $ 
    color col $ font (show fz++"px 'Klee One', cursive") $ text (x,y) str

putTextV :: Canvas -> [Bitmap] -> Color -> TxType -> Fsize -> Size 
                                                -> Point -> String -> IO ()
putTextV c wbmp col tp fz sz (p,q) = 
              putLettersV c wbmp col False tp fz sz q 0 (p,q) 

putLettersV :: Canvas -> [Bitmap] -> Color -> Bool -> TxType -> Fsize -> Size
                     -> Double -> Int -> Point -> String -> IO ()
putLettersV _ _ _ _ _ _ _ _ _ _ [] = return ()
putLettersV c wbmp col ie tp fz sz@(w,h) miq cln (pd,qd) (x:xs) = do
  let fzD = fromIntegral fz 
      ltw = fzD * 1.2
      lth = fzD * 1.1 
      mll = floor (h/lth) - 2 -- max letter length
  case x of 
    '\r'  -> putLettersV c wbmp col ie tp fz sz miq 0 (pd-ltw,miq) xs
    '：'  -> do
        let (rubi,xxs) = getRubi xs 
            (rpd,rqd) = getRubiPos (pd,qd) miq ltw lth fzD mll
            rfz = div fz 3
            rfzD = fromIntegral rfz
            rlth = rfzD * 1.1
        mapM_ (\(i,ch)-> putLet c col rfz 0 (rpd,rqd+i*rlth) ch) (zip [0..] rubi) 
        putLettersV c wbmp col ie tp fz sz miq cln (pd,qd) xxs
    _     -> do 
        case tp of
          Normal -> do
            let ird = x `elem` "ー<>" 
                rd = if ird then pi/2 else 0
                (pd',qd') = if ird then (pd+fzD/5,qd-fzD*3/4) else (pd,qd)
                col' = if not ie || x=='>' then col else chColors!!2
            putLet c col' fz rd (pd',qd') x  
          Osite -> putWst c wbmp fz (pd-fzD/6,qd-fzD*3/4) x 
        let isMax = cln > mll
        let ncln = if isMax then 0 else cln+1
        let npd = if isMax then pd-ltw else pd
        let nqd = if isMax then miq else qd+lth 
        let nie
             | x=='<' = True 
             | x=='>' = False
             | otherwise = ie
        putLettersV c wbmp col nie tp fz sz miq ncln (npd,nqd) xs

getRubiPos :: Point -> Double -> Double -> Double -> Double -> Int -> Point 
getRubiPos (pd,qd) miq ltw lth fzD mll
  | qd==miq = (pd+ltw+fzD*1.1,miq+(fromIntegral mll-1)*lth+fzD/3) 
  | otherwise = (pd+fzD*1.1,qd-lth*2+fzD/3*2)

getRubi :: String -> (String,String)
getRubi str = if '：' `elem` str 
                  then let (a,b) = break (=='：') str in (a,tail b)
                  else ("",str)

putWst :: Canvas -> [Bitmap] -> Fsize -> Point -> Char -> IO () 
putWst c wsts fs (x,y) ch = do
  let sc = fromIntegral fs/fromIntegral nfs -- nfs: normal font size
  renderOnTop c $ translate (x,y) $ scale (sc,sc) $ draw (wsts!!ind) (0,0)
    where ind = if ch `elem` wstIndex then getIndex ch wstIndex else 14

putLet :: Canvas -> Color -> Fsize -> Double -> Point -> Char -> IO ()
putLet c col fs rd (x,y) ch = do
  renderOnTop c $ color col$font (show fs++"px 'Klee One', cursive")$
--  renderOnTop c $ color col$font (show fs++"px IPAGothic")$
    translate (x,y)$rotate rd$text (0,0) [ch]

roundRect :: Point -> Vector -> Shape ()
roundRect (x,y) (w,h) = do  
  arc (x+w-10,y+h-10) 10 0 (pi*0.5)
  arc (x+10,y+h-10) 10 (pi*0.5) pi
  arc (x+10,y+10) 10 pi (pi*1.5)
  arc (x+w-10,y+10) 10 (pi*1.5) (pi*2)
  line (x+w,y+h-10) (x+w,y+10)
