{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE ViewPatterns   #-}

-- |
-- Module    : Data.Kanji.Types
-- Copyright : (c) Colin Woodbury, 2015, 2016
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Types for this library. While a constructor for `Kanji` is made available
-- here, you should prefer the `kanji` "smart constructor" unless you know
-- for sure that the `Char` in question falls within the correct UTF8 range.

module Data.Kanji.Types where

import           Control.DeepSeq (NFData)
import           Data.Aeson
import           Data.Aeson.Encoding (text)
import           Data.Bool (bool)
import           Data.Char (isLetter, isNumber, isPunctuation, ord)
import           Data.Hashable
import qualified Data.Text as T
import           GHC.Generics

---

-- | A single symbol of Kanji. Japanese Kanji were borrowed from China
-- over several waves during the last 1,500 years. Japan names 2,136 of
-- these as their standard set, with rarer characters being the domain
-- of academia and esoteric writers.
--
-- Japanese has several Japan-only Kanji, including:
--
-- * 畑 (a type of rice field)
-- * 峠 (a narrow mountain pass)
-- * 働 (to do physical labour)
newtype Kanji = Kanji Char deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, Hashable, NFData)

-- | The original `Char` of a `Kanji`.
_kanji :: Kanji -> Char
_kanji (Kanji k) = k

-- | Construct a `Kanji` value from some `Char` if it falls in the correct UTF8 range.
kanji :: Char -> Maybe Kanji
kanji c = bool Nothing (Just $ Kanji c) $ isKanji c

-- | A Level or "Kyuu" (級) of Japanese Kanji ranking. There are 12 of these,
-- from 10 to 1, including intermediate levels between 3 and 2, and 2 and 1.
--
-- Japanese students will typically have Level-5 ability by the time they
-- finish elementary school. Level-5 accounts for 1,006 characters.
--
-- By the end of middle school, they would have covered up to Level-3
-- (1607 Kanji) in their Japanese class curriculum.
--
-- While Level-2 (2,136 Kanji) is considered "standard adult" ability,
-- many adults could not pass the Level-2, or even the Level-Pre2 (1940 Kanji)
-- exam without considerable study.
--
-- Level data for Kanji above Level-2 is currently not provided by
-- this library.
data Level = Ten | Nine | Eight | Seven | Six | Five | Four | Three | PreTwo
           | Two | PreOne | One | Unknown
           deriving (Eq, Ord, Enum, Show, Generic, Hashable, NFData, ToJSON, FromJSON)

instance ToJSONKey Level where
  toJSONKey = ToJSONKeyText f g
    where f = T.pack . show
          g = text . T.pack . show

-- | Legal Kanji appear between UTF-8 characters 19968 and 40959.
isKanji :: Char -> Bool
isKanji (ord -> c) = 19968 <= c && c <= 40959
{-# INLINE isKanji #-}

-- | あ to ん.
isHiragana :: Char -> Bool
isHiragana (ord -> c) = 0x3040 <= c && c <= 0x309f
{-# INLINE isHiragana #-}

-- | ア to ン.
isKatakana :: Char -> Bool
isKatakana (ord -> c) = 0x30a0 <= c && c <= 0x30ff
{-# INLINE isKatakana #-}

-- | General categories for characters, at least as is useful for thinking about
-- Japanese.
--
-- Japanese "full-width" numbers and letters will be counted as `Numeral`
-- and `RomanLetter` respectively, alongside their usual ASCII forms.
data CharCat = Hanzi | Hiragana | Katakana | Numeral | RomanLetter | Punctuation | Other
  deriving (Eq, Ord, Show, Generic, Hashable, NFData, ToJSON, FromJSON)

category :: Char -> CharCat
category c | isKanji c       = Hanzi
           | isHiragana c    = Hiragana
           | isKatakana c    = Katakana
           | isLetter c      = RomanLetter
           | isNumber c      = Numeral
           | isPunctuation c = Punctuation
           | otherwise       = Other

instance ToJSONKey CharCat where
  toJSONKey = ToJSONKeyText f g
    where f = T.pack . show
          g = text . T.pack . show
