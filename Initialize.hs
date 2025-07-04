module Initialize where

import qualified Data.Map as M
import Define(State(..),Switch(..),Con(..),CRect(..),Bord(..)
             ,Board(..),BMode(..),BEvent(..),Sound(..),LSA(..)
             ,Event(..),Stage(..),TxType(..),Score(..),MType(..),ltQuestSrc)

initState :: State
initState = State {stage=Nothing
                  ,mtype=NoMission
                  ,level=0
                  ,score=Score 0 0
                  ,hiscs=replicate 19 0
                  ,quest=Nothing
                  ,seAu=[]
                  ,cons=[testCon]
                  ,gaus=[]
                  ,board=initBoard
                  ,dcon=Nothing
                  ,qsrc=ltQuestSrc
                  ,cli=[]
                  ,lsa=NoLSA
                  ,rgn=0
                  ,swc=initSwitch
                  ,db=""
                  }

initBoard :: Board
initBoard = Board NoB (0,0) 1 0 NoEvent

initSwitch :: Switch
initSwitch = Switch {ita=False
                    ,ils=False
                    ,igc=False
                    ,itc=False
                    ,ini=False
                    ,ias=False
                    }

testCon :: Con
testCon = emCon {cRec = CRect 80 100 200 370 
                ,border = Round
                ,txtPos = [(100,30)]
                ,txtFsz = [30]
                ,txtCos = [1]
                ,txts = ["こんにちは\n元氣ですか？"]
                ,typs = [Normal]
                ,clEv = Intro 
                }

emCon :: Con
emCon = Con {conID = 0
            ,cRec = CRect 0 0 10 10
            ,gSize = (10,15)
            ,border = NoBord
            ,borCol = 0
            ,filCol = 5
            ,txtPos = []
            ,picPos = []
            ,ponPos = []
            ,ponCos = []
            ,txtFsz = []
            ,txtCos = []
            ,txtDir = []
            ,txts = []
            ,typs = []
            ,picSize = []
            ,picNums = []
            ,audio = Nothing
            ,clEv = NoEvent
            ,visible = True
            ,enable = True
            }
