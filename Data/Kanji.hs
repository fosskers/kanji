module Data.Kanji
       (
         -- * Kanji
         Kanji(..)
       , kanji
       , allKanji
       , allToKanji
       , isKanji
       , hasLevel
       , kanjiDensity
       , elementaryKanjiDensity
       , percentSpread
         -- * Levels
       , Level(..)
       , Rank
       , rankNums
       , level
       , levels
       , isKanjiInLevel
       , levelDist
       , levelFromRank
       , averageLevel
       ) where

import           Data.Char (ord)
import           Data.List (sort, group)
import qualified Data.Set as S
import           Lens.Micro

import           Data.Kanji.TenthQ
import           Data.Kanji.NinthQ
import           Data.Kanji.EighthQ
import           Data.Kanji.SeventhQ
import           Data.Kanji.SixthQ
import           Data.Kanji.FifthQ
import           Data.Kanji.FourthQ
import           Data.Kanji.ThirdQ
import           Data.Kanji.PreSecondQ
import           Data.Kanji.SecondQ

---

-- | A single symbol of Kanji. Japanese Kanji were borrowed from China
-- over several waves during the past millenium. Japan names 2136 of
-- these as their standard set, with rarer characters being the domain
-- of academia and esoteric writers.
--
-- Japanese has several Japan-only Kanji, including:
--
-- * 畑 (a type of rice field)
-- * 峠 (a narrow mountain pass)
-- * 働 (to do physical labour)
newtype Kanji = Kanji { _kanji :: Char } deriving (Eq, Ord, Show)

-- | Traverse into a `Char` to find a `Kanji`.
kanji :: Traversal' Char Kanji
kanji f c = if isKanji c then _kanji <$> f (Kanji c) else pure c

-- | A Level or "Kyuu" (級) of Japanese Kanji ranking. There are 12 of these,
-- from 10 to 1, including intermediate levels between 3 and 2, and 2 and 1.
--
-- Japanese students will typically have Level-5 ability by the time they
-- finish elementary school. Level-5 accounts for 1006 characters.
--
-- By the end of middle school, they would have covered up to Level-3
-- (1607 Kanji) in their Japanese class curriculum.
--
-- While Level-2 (2136 Kanji) is considered "standard adult" ability,
-- many adults could not pass the Level-2, or even the Level-Pre2 (1940 Kanji)
-- exam without considerable study.
--
-- Level data for Kanji above Level-2 is currently not provided by
-- this library.
data Level = Level { _allKanji :: S.Set Kanji
                   , _rank :: Rank
                   } deriving (Eq, Show)

-- | A numeric representation of a `Level`.
type Rank = Float

-- | Numerical representations of the 12 ranks.
rankNums :: [Rank]
rankNums = [10,9,8,7,6,5,4,3,2.5,2,1.5,1]

-- | All Kanji, grouped by their Level (級) in ascending order.
-- Here, ascending order means from the lowest to the highest level,
-- meaning from 10 to 1.
allKanji :: [[Kanji]]
allKanji =  map allToKanji ks
  where ks = [tenthQ, ninthQ, eighthQ, seventhQ, sixthQ,
              fifthQ, fourthQ, thirdQ, preSecondQ, secondQ]

-- | Convert as many `Char` as possible into legal `Kanji`.
allToKanji :: [Char] -> [Kanji]
allToKanji ks = ks ^.. traverse . kanji

-- | Legal Kanji appear between UTF8 characters 19968 and 40959.
isKanji :: Char -> Bool
isKanji c = lowLimit <= c' && c' <= highLimit
    where c' = ord c
          lowLimit  = 19968  -- This is `一`
          highLimit = 40959  -- I don't have the right fonts to display this.

-- | Is the `Level` of a given `Kanji` known?
hasLevel :: [Level] -> Kanji -> Bool
hasLevel qs k = has _Just $ level qs k

-- | What is the density @d@ of Kanji characters in a given String,
-- where @0 <= d <= 1@?
kanjiDensity :: [Char] -> Float
kanjiDensity ks = length' (allToKanji ks) / length' ks
  where length' = fromIntegral . length

-- | As above, but only Kanji of the first 1006 are counted (those learned
-- in elementary school in Japan).
elementaryKanjiDensity :: [Char] -> Float
elementaryKanjiDensity ks = foldl (\acc (_,p) -> acc + p) 0 elementaryQs
  where elementaryQs  = filter (\(qn,_) -> qn `elem` [5..10]) distributions
        distributions = levelDist levels $ allToKanji ks

makeLevel :: [Kanji] -> Rank -> Level
makeLevel ks n = Level (S.fromDistinctAscList ks) n

-- | All `Level`s, with all their `Kanji`, ordered from Level-10 to Level-2.
levels :: [Level]
levels = map f $ zip allKanji rankNums
  where f (ks,n) = makeLevel ks n

-- | What `Level` does a Kanji belong to?
level :: [Level] -> Kanji -> Maybe Level
level [] _     = Nothing
level (q:qs) k | isKanjiInLevel q k = Just q
               | otherwise = level qs k

-- | Does a given `Kanji` belong to the given `Level`?
isKanjiInLevel :: Level -> Kanji -> Bool
isKanjiInLevel q k = S.member k $ _allKanji q

-- | Some Kanji may be outside the Level system. This means they are
-- particularly difficult, and are given a rank of 0 here.
-- This is used internally for averaging calculations.
rank :: [Level] -> Kanji -> Rank
rank qs k = maybe 0 _rank $ level qs k

-- | Is there a `Level` that corresponds with a given `Rank` value?
levelFromRank :: [Level] -> Rank -> Maybe Level
levelFromRank [] _      = Nothing
levelFromRank (q:qs) qn | _rank q == qn = Just q
                        | otherwise     = levelFromRank qs qn

-- | Find the average `Level` of a given set of `Kanji`.
averageLevel :: [Level] -> [Kanji] -> Float
averageLevel qs ks = average $ map (rank qs) ks
  where average ns = (sum ns) / (fromIntegral $ length ns) 

-- | How much of each `Level` is represented by a group of Kanji?
levelDist :: [Level] -> [Kanji] -> [(Rank,Float)]
levelDist qs ks = map toNumPercentPair $ group sortedRanks
  where sortedRanks = sort $ map (rank qs) ks
        toNumPercentPair qns = (head qns, length' qns / length' sortedRanks)
        length' n = fromIntegral $ length n

-- | The distribution of each `Kanji` in a set of them.
-- The distribution values must sum to 1.
percentSpread :: [Kanji] -> [(Kanji,Float)]
percentSpread ks = map getPercent kQuants
    where getPercent (k,q) = (k, fromIntegral q / totalKanji)
          kQuants    = kanjiQuantities ks
          totalKanji = fromIntegral $ foldl (\acc (_,q) -> q + acc) 0 kQuants

-- | Determines how many times each `Kanji` appears in given set of them.
kanjiQuantities :: [Kanji] -> [(Kanji,Int)]
kanjiQuantities = map (\ks -> (head ks, length ks)) . group . sort
