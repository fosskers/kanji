-- KanjiQ
-- Library for discerning what 級 (level) a given Kanji is.
-- 級 is pronounced `kyuu`, hence the appearance of the 
-- letter `q` in this library.

{- ON Q NUMBERS
All level numbers will be stored as Doubles.
Levels 10 to 3 will essentially be integers,
while 準2 and above will be respectively:
2.5, 2, 1.5, 1
-}

{- 漢検漢字と教育漢字
There is an important link:
Level 10 to level 5 perfectly match
the elementary school curriculum Kanji
for each grade year.
-}

module KanjiQ where

import qualified Data.Set as S

type Kanji = String

data Q = Q {allKanji :: S.Set Kanji, qNumber :: Double} deriving (Eq, Show)

makeQ :: [Kanji] -> Double -> Q
makeQ ks n = Q (S.fromDistinctAscList ks) n

-- Lists of Kanji of various levels.
-- USE `fromDistinctAscList` !!!
tenthQ     = undefined
ninthQ     = undefined
eigthQ     = undefined
seventhQ   = undefined
sixthQ     = undefined
fifthQ     = undefined
forthQ     = undefined
thirdQ     = undefined
preSecondQ = undefined
secondQ    = undefined
preFirstQ  = undefined
firstQ     = undefined

allQs :: [Q]
allQs = [tenthQ, ninthQ, eigthQ, seventhQ, sixthQ, fifthQ, forthQ
        ,thirdQ, preSecondQ, secondQ, preFirstQ, firstQ]

kanjiInQ :: Kanji -> Q -> Bool
kanjiInQ k q = S.member k . allKanji $ q

whatQ :: Kanji -> Either String Double
whatQ k = checkQs allQs
    where checkQs (q:qs) = if kanjiInQ k q
                           then Right $ qNumber q
                           else checkQs qs
          checkQs []     = Left $ k ++ " is not in any 級"