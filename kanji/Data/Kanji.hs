{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

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
         -- * Levels
       , Level(..)
       , Rank(..)
       , level
       , levels
       , isKanjiInLevel
       , levelFromRank
         -- * Analysis
       , percentSpread
       , levelDist
       , averageLevel
         -- ** Densities
       , kanjiDensity
       , elementaryDen
       , middleDen
       , highDen
       , adultDen
       ) where

import           Control.Arrow hiding (second)
import           Data.Bool (bool)
import           Data.List (sort, group)
import qualified Data.Set as S
import           Lens.Micro

import           Data.Kanji.Levels
import           Data.Kanji.Types

---

-- | All Japanese Kanji, grouped by their Level (級) in ascending order.
-- Here, ascending order means from the lowest to the highest level,
-- meaning from 10 to 1.
allKanji :: [S.Set Kanji]
allKanji =  map f ks
  where f = foldr (\k s -> bool s (S.insert (Kanji k) s) $ isKanji k) mempty
        ks = [tenth, ninth, eighth, seventh, sixth,
              fifth, fourth, third, preSecond, second]

-- | Is the `Level` of a given `Kanji` known?
hasLevel :: Kanji -> Bool
hasLevel k = has _Just $ level k

-- | What is the density @d@ of Kanji characters in a given String-like
-- type, where @0 <= d <= 1@?
kanjiDensity :: Int -> [Kanji] -> Float
kanjiDensity len ks = fromIntegral (length ks) / fromIntegral len

-- | How much of the Kanji found are learned in elementary school in Japan?
--
-- > elementaryDen . levelDist :: [Kanji] -> Float
elementaryDen :: [(Rank,Float)] -> Float
elementaryDen dists = sum $ dists ^.. each . inRank [Five, Six ..]

-- | How much of the Kanji found are learned by the end of middle school?
--
-- > middleDen . levelDist :: [Kanji] -> Float
middleDen :: [(Rank,Float)] -> Float
middleDen dists = sum $ dists ^.. each . inRank [Three, Four ..]

-- | How much of the Kanji found are learned by the end of high school?
--
-- > highDen . levelDist :: [Kanji] -> Float
highDen :: [(Rank,Float)] -> Float
highDen dists = sum $ dists ^.. each . inRank [PreTwo, Three ..]

-- | How much of the Kanji found should be able to read by the average person?
--
-- > adultDen . levelDist :: [Kanji] -> Float
adultDen :: [(Rank,Float)] -> Float
adultDen dists = sum $ dists ^.. each . inRank [Two, PreTwo ..]

inRank :: [Rank] -> Traversal' (Rank,Float) Float
inRank rs f (r,n) | r `elem` rs = (r,) <$> f n
                  | otherwise = pure (r,n)

-- | All `Level`s, with all their `Kanji`, ordered from Level-10 to Level-2.
levels :: [Level]
levels = zipWith Level allKanji [Ten ..]

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
        average ns = sum ns / fromIntegral (length ns)

-- | How much of each `Level` is represented by a group of Kanji?
levelDist :: [Kanji] -> [(Rank,Float)]
levelDist ks = map toNumPercentPair $ group sortedRanks
  where sortedRanks = sort $ ks ^.. each . to level . _Just . to _rank
        toNumPercentPair qns = (head qns, length' qns / length' ks)
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
kanjiQuantities = map (head &&& length) . group . sort
