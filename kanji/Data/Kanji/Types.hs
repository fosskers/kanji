{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module    : Data.Kanji.Types
-- Copyright : (c) Colin Woodbury, 2015, 2016
-- License   : GPL3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- Types for this library. Note that typeclass instances for `ByteString`s
-- are not provided, as `SB.pack` garbles characters above 0xFF.

module Data.Kanji.Types where

import           Data.Char (ord)
import           Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import           Lens.Micro

---

-- | Anything that can be transformed into a list of Kanji.
class AsKanji a where

  -- | Traverse into this type to find 0 or more Kanji.
  --
  -- Despite what the Haddock documentation says, this is part of the
  -- minimal complete definition.
  _Kanji :: Traversal' a Kanji

  -- | How long is this input source?
  len :: Num b => a -> b

  -- | Transform this string type into a list of Kanji. The source string
  -- and the resulting list might not have the same length, if there
  -- were `Char` in the source that did not fall within the legal
  -- UTF8 range for Kanji.
  asKanji :: a -> [Kanji]
  asKanji a = a ^.. _Kanji

instance AsKanji Char where
  _Kanji f c = if isKanji c then _kanji <$> f (Kanji c) else pure c
  {-# INLINE _Kanji #-}

  len = const 1

instance AsKanji [Char] where
  _Kanji = traverse . _Kanji
  {-# INLINE _Kanji #-}

  len = fromIntegral . length

instance AsKanji ST.Text where
  _Kanji = packed . _Kanji
    where packed f b = ST.pack <$> f (ST.unpack b)
  {-# INLINE _Kanji #-}

  len = fromIntegral . ST.length

instance AsKanji LT.Text where
  _Kanji = packed . _Kanji
    where packed f b = LT.pack <$> f (LT.unpack b)
  {-# INLINE _Kanji #-}

  len = fromIntegral . LT.length

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
data Rank = Ten | Nine | Eight | Seven | Six | Five | Four | Three | PreTwo
  | Two | PreOne | One deriving (Eq, Ord, Enum, Read, Show)

-- | Discover a Rank's numeric representation, as a `Float`.
fromRank :: Rank -> Float
fromRank = fromJust . flip lookup rankMap

-- | A mapping of Ranks to their numeric representation.
rankMap :: [(Rank,Float)]
rankMap = zip [Ten ..] [10,9,8,7,6,5,4,3,2.5,2,1.5,1]

-- | Legal Kanji appear between UTF8 characters 19968 and 40959.
isKanji :: Char -> Bool
isKanji c = lowLimit <= c' && c' <= highLimit
    where c' = ord c
          lowLimit  = 19968  -- This is `一`
          highLimit = 40959  -- I don't have the right fonts to display this.
