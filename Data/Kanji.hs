-- |
-- Module    : Data.Kanji
-- Copyright : (c) Colin Woodbury, 2015, 2016
-- License   : GPL3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- A library for analysing the density of Kanji in given texts,
-- according to their "Level" classification, as defined by the
-- Japan Kanji Aptitude Testing Foundation (日本漢字能力検定協会).

module Data.Kanji
       (
         -- * Kanji
         AsKanji(..)
       , Kanji(..)
       , allKanji
       , isKanji
       , hasLevel
       , kanjiDensity
       , elementaryKanjiDensity
       , percentSpread
         -- * Levels
       , Level(..)
       , Rank(..)
       , level
       , levels
       , isKanjiInLevel
       , levelDist
       , levelFromRank
       , averageLevel
       ) where

import           Data.List (sort, group)
import qualified Data.Set as S
import           Lens.Micro

import           Data.Kanji.Level.EighthQ
import           Data.Kanji.Level.FifthQ
import           Data.Kanji.Level.FourthQ
import           Data.Kanji.Level.NinthQ
import           Data.Kanji.Level.PreSecondQ
import           Data.Kanji.Level.SecondQ
import           Data.Kanji.Level.SeventhQ
import           Data.Kanji.Level.SixthQ
import           Data.Kanji.Level.TenthQ
import           Data.Kanji.Level.ThirdQ
import           Data.Kanji.Types

---

-- | All Kanji, grouped by their Level (級) in ascending order.
-- Here, ascending order means from the lowest to the highest level,
-- meaning from 10 to 1.
allKanji :: [[Kanji]]
allKanji =  map asKanji ks
  where ks = [tenthQ, ninthQ, eighthQ, seventhQ, sixthQ,
              fifthQ, fourthQ, thirdQ, preSecondQ, secondQ]

-- | Is the `Level` of a given `Kanji` known?
hasLevel :: Kanji -> Bool
hasLevel k = has _Just $ level k

-- | What is the density @d@ of Kanji characters in a given String-like
-- type, where @0 <= d <= 1@?
kanjiDensity :: AsKanji a => a -> [Kanji] -> Float
kanjiDensity orig ks = fromIntegral (length ks) / len orig

-- | As above, but only Kanji of the first 1006 are counted (those learned
-- in elementary school in Japan).
elementaryKanjiDensity :: [Kanji] -> Float
elementaryKanjiDensity ks = foldl (\acc (_,p) -> acc + p) 0 elementaryQs
  where elementaryQs  = filter (\(qn,_) -> qn `elem` [Five, Six ..]) dists
        dists = levelDist ks

makeLevel :: [Kanji] -> Rank -> Level
makeLevel ks n = Level (S.fromDistinctAscList ks) n

-- | All `Level`s, with all their `Kanji`, ordered from Level-10 to Level-2.
levels :: [Level]
levels = map f $ zip allKanji [Ten ..]
  where f (ks,n) = makeLevel ks n

-- | What `Level` does a Kanji belong to?
level :: Kanji -> Maybe Level
level = level' levels
  where level' [] _     = Nothing
        level' (q:qs) k | isKanjiInLevel q k = Just q
                        | otherwise = level' qs k

-- | Does a given `Kanji` belong to the given `Level`?
isKanjiInLevel :: Level -> Kanji -> Bool
isKanjiInLevel q k = S.member k $ _allKanji q

-- | Is there a `Level` that corresponds with a given `Rank` value?
levelFromRank :: Rank -> Maybe Level
levelFromRank = levelFromRank' levels
  where levelFromRank' [] _      = Nothing
        levelFromRank' (q:qs) qn | _rank q == qn = Just q
                                 | otherwise     = levelFromRank' qs qn

-- | Find the average `Level` of a given set of `Kanji`.
averageLevel :: [Kanji] -> Float
averageLevel ks = average ranks
  where ranks = ks ^.. each . to level . _Just . to _rank . to fromRank
        average ns = (sum ns) / (fromIntegral $ length ns) 

-- | How much of each `Level` is represented by a group of Kanji?
levelDist :: [Kanji] -> [(Rank,Float)]
levelDist ks = map toNumPercentPair $ group sortedRanks
  where sortedRanks = sort $ ks ^.. each . to level . _Just . to _rank
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
