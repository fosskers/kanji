{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module    : Data.Kanji
-- Copyright : (c) Colin Woodbury, 2015 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- A library for analysing the density of Kanji in given texts,
-- according to their "Level" classification, as defined by the
-- Japan Kanji Aptitude Testing Foundation (日本漢字能力検定協会).

module Data.Kanji
       (
         -- * Kanji
         Kanji
       , kanji, _kanji
       , allKanji
       , isKanji
       , hasLevel
         -- * Levels
       , Level(..)
       , level
         -- * Analysis
       , percentSpread
       , levelDist
       , averageLevel
       , uniques
         -- ** Densities
       , kanjiDensity
       , elementaryDen
       , middleDen
       , highDen
       , adultDen
       ) where

import           Control.Applicative ((<|>))
import           Control.Arrow hiding (second)
import           Data.Bool (bool)
import           Data.Kanji.Levels
import           Data.Kanji.Types
import           Data.List (sort, group)
import qualified Data.Map.Strict as M
import           Data.Maybe (isJust)
import qualified Data.Set as S

---

-- | All Japanese Kanji, grouped by their Level (級).
allKanji :: M.Map Level (S.Set Kanji)
allKanji =  M.fromList . zip [ Ten .. ] $ map (S.map Kanji) ks
  where ks = [ tenth, ninth, eighth, seventh, sixth
             , fifth, fourth, third, preSecond, second ]

-- | Is the `Level` of a given `Kanji` known?
hasLevel :: Kanji -> Bool
hasLevel = isJust . level

-- | What is the density @d@ of Kanji characters in a given String-like
-- type, where @0 <= d <= 1@?
kanjiDensity :: Int -> [Kanji] -> Float
kanjiDensity len ks = fromIntegral (length ks) / fromIntegral len

-- | How much of the Kanji found are learnt in elementary school in Japan?
--
-- > elementaryDen . levelDist :: [Kanji] -> Float
elementaryDen :: M.Map Level Float -> Float
elementaryDen = undefined
-- elementaryDen dists = sum $ dists ^.. each . inRank [Five, Six ..]

-- | How much of the Kanji found are learnt by the end of middle school?
--
-- > middleDen . levelDist :: [Kanji] -> Float
middleDen :: M.Map Level Float -> Float
middleDen = undefined
-- middleDen dists = sum $ dists ^.. each . inRank [Three, Four ..]

-- | How much of the Kanji found are learnt by the end of high school?
--
-- > highDen . levelDist :: [Kanji] -> Float
highDen :: M.Map Level Float -> Float
highDen = undefined
-- highDen dists = sum $ dists ^.. each . inRank [PreTwo, Three ..]

-- | How much of the Kanji found should be able to read by the average person?
--
-- > adultDen . levelDist :: [Kanji] -> Float
adultDen :: M.Map Level Float -> Float
adultDen = undefined
-- adultDen dists = sum $ dists ^.. each . inRank [Two, PreTwo ..]

-- inRank :: [Rank] -> Traversal' (Rank,Float) Float
-- inRank rs f (r,n) | r `elem` rs = (r,) <$> f n
--                   | otherwise = pure (r,n)

-- | What `Level` does a Kanji belong to?
level :: Kanji -> Maybe Level
level k = M.foldlWithKey' (\acc l ks -> acc <|> bool Nothing (Just l) (S.member k ks)) Nothing allKanji

-- | Find the average `Level` of a given set of `Kanji`.
averageLevel :: [Kanji] -> Float
averageLevel = undefined
-- averageLevel ks = average ranks
--   where ranks = undefined -- ks ^.. each . to level . _Just . to _rank . to numericLevel
--         average ns = sum ns / fromIntegral (length ns)

-- | How much of each `Level` is represented by a group of Kanji?
levelDist :: [Kanji] -> M.Map Level Float
levelDist = undefined
-- levelDist ks = map toNumPercentPair $ group sortedRanks
--   where sortedRanks = undefined -- sort $ ks ^.. each . to level . _Just . to _rank
--         toNumPercentPair qns = (head qns, length' qns / length' ks)
--         length' n = fromIntegral $ length n

-- | The distribution of each `Kanji` in a set of them.
-- The distribution values must sum to 1.
percentSpread :: [Kanji] -> M.Map Kanji Float
percentSpread ks = getPercent <$> kQuants
    where getPercent q = fromIntegral q / totalKanji
          kQuants      = kanjiQuantities ks
          totalKanji   = fromIntegral $ M.foldl' (+) 0 kQuants

-- | Determines how many times each `Kanji` appears in given set of them.
kanjiQuantities :: [Kanji] -> M.Map Kanji Int
kanjiQuantities = M.fromList . map (head &&& length) . group . sort

-- | Which Kanji appeared from each Level in the text?
uniques :: [Kanji] -> M.Map Level [Kanji]
uniques = S.foldl' h M.empty . S.fromList
  where h a k = maybe a (\l -> M.insertWith (++) l [k] a) $ level k
