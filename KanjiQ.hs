-- KanjiQ
-- Library for discerning what 級 (level) a given Kanji is.
-- 級 is pronounced `kyuu`, hence the appearance of the 
-- letter `q` in this library.

module KanjiQ where

import qualified Data.Set as S

data Kanji = Kanji Char deriving (Eq, Ord)

instance Show Kanji where
    show (Kanji c) = [c]

data Q = Q {allKanjiInSet :: S.Set Kanji, qNumber :: Double} deriving (Eq, Show)

makeQ :: [Kanji] -> Double -> Q
makeQ ks n = Q (S.fromDistinctAscList ks) n

-- Base path to the Kanji data files.
basePath :: String 
basePath = "./data/"

kanjiFiles :: [String]
kanjiFiles = ["tenthQ.txt", "ninthQ.txt", "eigthQ.txt", "seventhQ.txt",
              "sixthQ.txt", "fifthQ.txt"]

kanjiFilePaths :: [String]
kanjiFilePaths = map (basePath ++) kanjiFiles

qNumbers :: [Double]
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

toKanji :: Char -> Kanji
toKanji k  = Kanji k

whatQ :: Kanji -> [Q] -> Maybe Double
whatQ k qs = checkQs qs
    where checkQs (q:qs') = if kanjiInQ k q
                            then Just $ qNumber q
                            else checkQs qs'
          checkQs []      = Nothing

kanjiInQ :: Kanji -> Q -> Bool
kanjiInQ k q = S.member k . allKanjiInSet $ q
