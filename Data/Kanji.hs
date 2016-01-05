module Data.Kanji where

import Data.Kanji.TenthQ
import Data.Kanji.NinthQ
import Data.Kanji.EighthQ
import Data.Kanji.SeventhQ
import Data.Kanji.SixthQ
import Data.Kanji.FifthQ
import Data.Kanji.FourthQ
import Data.Kanji.ThirdQ
import Data.Kanji.PreSecondQ
import Data.Kanji.SecondQ

allKanji :: [String]
allKanji = [tenthQ, ninthQ, eighthQ, seventhQ, sixthQ,
            fifthQ, fourthQ, thirdQ, preSecondQ, secondQ]
