{-# LANGUAGE TupleSections #-}

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
       , isKanji, isHiragana, isKatakana
         -- * Character Categories
       , CharCat(..)
       , category
         -- * Levels
       , Level(..)
       , level
         -- * Analysis
       , percentSpread
       , levelDist
       , uniques
         -- ** Densities
       , densities
       , elementaryDen
       , middleDen
       , highDen
       , adultDen
       ) where

import           Control.Arrow hiding (second)
import           Data.Foldable (fold)
import           Data.Kanji.Levels
import           Data.Kanji.Types
import           Data.List (sort, group)
import qualified Data.Map.Strict as M
import           Data.Semigroup ((<>))
import qualified Data.Set as S
import qualified Data.Text as T

---

-- | All Japanese `Kanji`, grouped by their Level (級).
allKanji :: M.Map Level (S.Set Kanji)
allKanji =  M.fromList . zip [ Ten .. ] $ map (S.map Kanji) ks
  where ks = [ tenth, ninth, eighth, seventh, sixth
             , fifth, fourth, third, preSecond, second ]

-- | All Japanese `Kanji` with their `Level`.
allKanji' :: M.Map Kanji Level
allKanji' = M.fromList . S.toList . fold $ M.mapWithKey (\k v -> S.map (,k) v) allKanji

-- | What `Level` does a Kanji belong to? `Unknown` for Kanji above level `Two`.
level :: Kanji -> Level
level k = maybe Unknown id $ M.lookup k allKanji'
{-# INLINE level #-}

-- | Percentage of appearance of each `CharCat` in the source text.
-- The percentages will sum to 1.0.
densities :: T.Text -> M.Map CharCat Float
densities t = M.fromList . map (head &&& f) . group . sort . map category $ T.unpack t
  where f xs = fromIntegral (length xs) / fromIntegral (T.length t)

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

-- | How much of each `Level` is represented by a group of Kanji?
-- The distribution values will sum to 1.
levelDist :: [Kanji] -> M.Map Level Float
levelDist ks = M.fromList . map percentPair . group . sort $ map level ks
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
  where h a k = (\l -> M.insertWith (<>) l (S.singleton k) a) $ level k
