-- KanjiQ
-- Library for discerning what 級 (level) a given Kanji is.
-- 級 is pronounced `kyuu`, hence the appearance of the 
-- letter `q` in this library.

module KanjiQ where

import qualified Data.Set as S
import KanjiData (allKanjiLists)
import Data.List (sort, group)    
import Data.Char (ord)

data Kanji = Kanji Char deriving (Eq, Ord)

instance Show Kanji where
    show (Kanji c) = [c]

type QNum = Double

data Q = Q {allKanjiInSetOf :: S.Set Kanji, qNumberOf :: QNum}
         deriving (Eq, Show)

makeQ :: [Kanji] -> QNum -> Q
makeQ ks n = Q (S.fromDistinctAscList ks) n

qNumbers :: [QNum]
qNumbers = [10,9,8,7,6,5,4,3,2.5,2,1.5,1]

allQs :: [Q]
allQs = map (\(ks,n) -> makeQ (map toKanji ks) n) pairs
    where pairs = zip allKanjiLists qNumbers

toKanji :: Char -> Kanji
toKanji k = if isKanji k then Kanji k else error $ k : " is not a Kanji!"

isKanji :: Char -> Bool
isKanji c = lowLimit <= c' && c' <= highLimit
    where c' = ord c
          lowLimit  = 19968  -- This is `一`
          highLimit = 40959  -- I don't have the right fonts to display this.

-- Find out what Level a Kanji belongs to.
whatQ :: [Q] -> Kanji -> Maybe QNum
whatQ qs k = checkQs qs
    where checkQs (q:qs') = if isKanjiInQ q k
                            then Just $ qNumberOf q
                            else checkQs qs'
          checkQs []      = Nothing

isKanjiInQ :: Q -> Kanji -> Bool
isKanjiInQ q k = S.member k . allKanjiInSetOf $ q

getQNum :: [Q] -> Kanji -> QNum
getQNum qs k = case whatQ qs k of
                 Just qn -> qn
                 Nothing -> 0  -- Not found means it's a tough Kanji.

getQ :: [Q] -> QNum -> Maybe Q
getQ [] _      = Nothing
getQ (q:qs) qn | qNumberOf q == qn = Just q
               | otherwise         = getQ qs qn

-- Find the average Level of a given set of Kanji.
averageQ :: [Q] -> [Kanji] -> Double
averageQ qs ks = average $ map (getQNum qs) ks
    where average ns = (sum ns) / (fromIntegral $ length ns) 

-- Determines how many times each Kanji appears in given set of them.
kanjiQuantities :: [Kanji] -> [(Kanji,Int)]
kanjiQuantities = map (\ks -> (head ks, length ks)) . group . sort

-- How much of each Level is represented by a group of Kanji?
qDistribution :: [Q] -> [Kanji] -> [(QNum,Float)]
qDistribution qs ks = map toNumPercentPair $ group sortedQNums
    where sortedQNums = sort $ map (getQNum qs) ks
          toNumPercentPair qns = (head qns, length' qns / length' sortedQNums)
          length' n = fromIntegral $ length n

-- Is the Level of a given Kanji known?
isKnown :: [Q] -> Kanji -> Bool
isKnown qs k = case whatQ qs k of
                   Just _  -> True
                   Nothing -> False

isUnknown :: [Q] -> Kanji -> Bool
isUnknown qs = not . isKnown qs

areSameQ :: [Q] -> Kanji -> Kanji -> Bool
areSameQ qs k1 k2 = compareQs (whatQ qs k1) (whatQ qs k2)
    where compareQs (Just q1) (Just q2) = q1 == q2
          compareQs _ _ = False

-- Gives the percent distribution of _each Kanji_ in a set of them.
percentSpread :: [Kanji] -> [(Kanji,Float)]
percentSpread ks = map getPercent kQuants
    where getPercent (k,q) = (k, 100 * (fromIntegral q) / totalKanji)
          kQuants    = kanjiQuantities ks
          totalKanji = fromIntegral $ foldl (\acc (_,q) -> q + acc) 0 kQuants