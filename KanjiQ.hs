-- KanjiQ
-- Library for discerning what 級 (level) a given Kanji is.
-- 級 is pronounced `kyuu`, hence the appearance of the 
-- letter `q` in this library.

module KanjiQ where

import qualified Data.Set as S
import Data.Char (ord)

data Kanji = Kanji Char deriving (Eq, Ord)

instance Show Kanji where
    show (Kanji c) = [c]

type QNum = Double

data Q = Q {allKanjiInSet :: S.Set Kanji, qNumber :: QNum} deriving (Eq, Show)

makeQ :: [Kanji] -> QNum -> Q
makeQ ks n = Q (S.fromDistinctAscList ks) n

-- Base path to the Kanji data files.
basePath :: String 
basePath = "./data/"

kanjiFiles :: [String]
kanjiFiles = ["tenthQ.txt", "ninthQ.txt", "eigthQ.txt", "seventhQ.txt",
              "sixthQ.txt", "fifthQ.txt", "fourthQ.txt", "thirdQ.txt"]

kanjiFilePaths :: [String]
kanjiFilePaths = map (basePath ++) kanjiFiles

qNumbers :: [QNum]
qNumbers = [10,9,8,7,6,5,4,3,2.5,2,1.5,1]

allQs :: IO [Q]
allQs = do
  allKanjiByQ <- readKanjiFiles
  let kanjiLists = map toKanjiList allKanjiByQ
      withQNums  = zip kanjiLists qNumbers
  return . map (\(ks,n) -> makeQ ks n) $ withQNums
      where toKanjiList = map (toKanji . head) . lines

readKanjiFiles :: IO [String]
readKanjiFiles = mapM readFile kanjiFilePaths

isKanji :: Char -> Bool
isKanji c = lowLimit <= c' && c' <= highLimit
    where c' = ord c
          lowLimit  = 19968  -- This is `一`
          highLimit = 40959  -- I don't have the right fonts to display this.

toKanji :: Char -> Kanji
toKanji k = if isKanji k then Kanji k else error $ k : " is not a Kanji!"

-- Find out what Level a Kanji belongs to.
whatQ :: [Q] -> Kanji -> Maybe QNum
whatQ qs k = checkQs qs
    where checkQs (q:qs') = if isKanjiInQ q k
                            then Just $ qNumber q
                            else checkQs qs'
          checkQs []      = Nothing

isKanjiInQ :: Q -> Kanji -> Bool
isKanjiInQ q k = S.member k . allKanjiInSet $ q

-- Find the average Level of a given set of Kanji.
averageQ :: [Q] -> [Kanji] -> Double
averageQ qs ks = average $ map getQNum ks
    where getQNum k = case whatQ qs k of
                        Just qn -> qn
                        Nothing -> 0  -- Not found means it's a tough Kanji.
          average ns = (sum ns) / (fromIntegral $ length ns) 

areSameQ :: [Q] -> Kanji -> Kanji -> Bool
areSameQ qs k1 k2 = compareQs (whatQ qs k1) (whatQ qs k2)
    where compareQs (Just q1) (Just q2) = q1 == q2
          compareQs _ _ = False
