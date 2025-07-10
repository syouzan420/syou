{-# LANGUAGE OverloadedStrings #-}
module Kanji(toMon) where

import qualified Data.Text as T
import File (fileRead, fileWrite)

inputFile :: FilePath
inputFile = "Texts/kanji.txt"
 
outputFile :: FilePath
outputFile = "KanjiM.hs"

getAns :: String -> String
getAns str =
  let res =  concatMap (\(i,w) ->
                   if even i then w else
                     if '-' `elem` w then (tail . snd) (break (=='-') w) else "")
                                                   (zip [0..] (words str)) 
   in "<"++res++">"
--  let res =  concatMap snd $ filter (\(i,_) -> even i) (zip [0..] (words str)) 
--   in "<"++res++">"

getMon :: String -> String
getMon str =
  let res = concatMap snd $ filter (\(i,_) -> odd i) (zip [0..] (words str)) 
   in "<"++removeChr '-' res++">"

removeChr :: Char -> String -> String
removeChr _ [] = []
removeChr ch (x:xs) = if ch==x then xs else x:removeChr ch xs 

getTgt :: String -> (String,String)
getTgt [] = ([],[])
getTgt (x:xs)
  | x=='>' = ([],xs)
  | otherwise = (x:(fst (getTgt xs)),snd (getTgt xs))

toMon :: String -> (String,String)
toMon [] = ([],[])
toMon (x:xs)
  | x=='<' = let (tgt,xxs) = getTgt xs
                 nxt = toMon xxs
              in (getMon tgt++fst nxt,getAns tgt++snd nxt)
  | otherwise = let nxt = toMon xs
                 in (x:fst nxt,x:snd nxt) 

conv :: IO ()
conv = do
  lns <- T.lines <$> fileRead inputFile
  let res = "module KanjiM (kanmons) where\n\nkanmons :: [(String,String)]\nkanmons = " <> (T.pack . show) (map (toMon . T.unpack) lns)
  fileWrite outputFile res


kj1="一右雨円王音下火花貝学気九休玉金空月犬見口校左三山子四糸字耳七車手十出女小上森人水正生青夕石赤千川先早足村大男竹中虫町天田土二日入年白八百文木本名目立力林六五"
kj2="引羽雲園遠何科夏家歌画回会海絵外角楽活間丸岩顔汽記帰弓牛魚京強教近兄形計元言原戸古午後語工公広交光考行高黄合谷国黒今才細作算止市矢姉思紙寺自時室社弱首秋週春書少場色食心新親図数西声星晴切雪船線前組走多太体台地池知茶昼長鳥朝直通弟店点電刀冬当東答頭同道読内南肉馬売買麦半番父風分聞米歩母方北毎妹万明鳴毛門夜野友用曜来里理話"
kj3="悪安暗医委意育員院飲運泳駅央横屋温化荷界開階寒感漢館岸起期客究急級宮球去橋業曲局銀区苦具君係軽血決研県庫湖向幸港号根祭皿仕死使始指歯詩次事持式実写者主守取酒受州拾終習集住重宿所暑助昭消商章勝乗植申身神真深進世整昔全相送想息速族他打対待代第題炭短談着注柱丁帳調追定庭笛鉄転都度投豆島湯登等動童農波配倍箱畑発反坂板皮悲美鼻筆氷表秒病品負部服福物平返勉放味命面問役薬由油有遊予羊洋葉陽様落流旅両緑礼列練路和"
kj4="愛案以衣位囲胃印英栄塩億加果貨課芽改械害街各覚完官管関観願希季紀喜旗器機議求泣救給挙漁共協鏡競極訓軍郡径型景芸欠結建健験固功好候航康告差菜最材昨札刷殺察参産散残士氏史司試児治辞失借種周祝順初松笑唱焼象照賞臣信成省清静席積折節説浅戦選然争倉巣束側続卒孫帯隊達単置仲貯兆腸低底停的典伝徒努灯堂働特得毒熱念敗梅博飯飛費必票標不夫付府副粉兵別辺変便包法望牧末満未脈民無約勇要養浴利陸良料量輪類令冷例歴連老労録"
kj5="圧囲移因永営衛易益液演応往桜可仮価河過快解格確額刊幹慣眼紀基寄規喜技義逆久旧救居許境均禁句型経潔件険検限現減故個護効厚耕航鉱構興講告混査再災妻採際在財罪殺雑酸賛士支史志枝師資飼示似識質舎謝授修述術準序招証象賞条状常情織職制性政勢精製税責績接設絶祖素総造像増則測属率損貸態団断築貯張停提程適統堂銅導得毒独任燃能破犯判版比肥非費備評貧布婦武復複仏粉編弁保墓報豊防貿暴脈務夢迷綿輸余容略留領歴"
kj6="胃異遺域宇映延沿恩我灰拡革閣割株干巻看簡危机揮貴疑吸供胸郷勤筋系敬警劇激穴券絹権憲源厳己呼誤后孝皇紅降鋼刻穀骨困砂座済裁策冊蚕至私姿視詞誌磁射捨尺若樹収宗就衆従縦縮熟純処署諸除承将傷障蒸針仁垂推寸盛聖誠舌宣専泉洗染銭善奏窓創装層操蔵臓存尊退宅担探誕段暖値宙忠著庁頂腸潮賃痛敵展討党糖届難乳認納脳派拝背肺俳班晩否批秘俵腹奮並陛閉片補暮宝訪亡忘棒枚幕密盟模訳郵優預幼欲翌乱卵覧裏律臨朗論"
