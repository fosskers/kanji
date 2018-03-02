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
import           Data.Foldable (foldl')
import           Data.Kanji.Levels
import           Data.Kanji.Types
import           Data.List (sort, group)
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes)
import           Data.Semigroup ((<>))
import qualified Data.Set as S

---

-- | All Japanese Kanji, grouped by their Level (級).
allKanji :: M.Map Level (S.Set Kanji)
allKanji =  M.fromList . zip [ Ten .. ] $ map (S.map Kanji) ks
  where ks = [ tenth, ninth, eighth, seventh, sixth
             , fifth, fourth, third, preSecond, second ]

-- | What `Level` does a Kanji belong to?
level :: Kanji -> Maybe Level
level k = M.foldlWithKey' (\acc l ks -> acc <|> bool Nothing (Just l) (S.member k ks)) Nothing allKanji

-- | Given the length of some String-like type and a list of `Kanji` found therein,
-- what percentage of them were Kanji?
kanjiDensity :: Int -> [Kanji] -> Float
kanjiDensity len ks = fromIntegral (length ks) / fromIntegral len

-- | How much of the Kanji found are learnt in elementary school in Japan?
--
-- > elementaryDen . levelDist :: [Kanji] -> Float
elementaryDen :: M.Map Level Float -> Float
elementaryDen m = M.foldl' (+) 0 . M.restrictKeys m $ S.fromList [ Five, Six .. ]

-- | How much of the Kanji found are learnt by the end of middle school?
--
-- > middleDen . levelDist :: [Kanji] -> Float
middleDen :: M.Map Level Float -> Float
middleDen m = M.foldl' (+) 0 . M.restrictKeys m $ S.fromList [ Three, Four .. ]

-- | How much of the Kanji found are learnt by the end of high school?
--
-- > highDen . levelDist :: [Kanji] -> Float
highDen :: M.Map Level Float -> Float
highDen m = M.foldl' (+) 0 . M.restrictKeys m $ S.fromList [ PreTwo, Three .. ]

-- | How much of the Kanji found should be able to be read by the average person?
--
-- > adultDen . levelDist :: [Kanji] -> Float
adultDen :: M.Map Level Float -> Float
adultDen m = M.foldl' (+) 0 . M.restrictKeys m $ S.fromList [ Two, PreTwo .. ]

-- | Find the average `Level` of a given set of `Kanji`.
averageLevel :: [Kanji] -> Float
averageLevel ks = average . map numericLevel . catMaybes $ map level ks
  where average ns = foldl' (+) 0 ns / fromIntegral (length ns)

-- | How much of each `Level` is represented by a group of Kanji?
-- The distribution values must sum to 1.
levelDist :: [Kanji] -> M.Map Level Float
levelDist ks = M.fromList . map percentPair . group . sort . catMaybes $ map level ks
  where percentPair qns = (head qns, fromIntegral (length qns) / totalKs)
        totalKs = fromIntegral $ length ks

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
uniques :: [Kanji] -> M.Map Level (S.Set Kanji)
uniques = S.foldl' h M.empty . S.fromList
  where h a k = maybe a (\l -> M.insertWith (<>) l (S.singleton k) a) $ level k
