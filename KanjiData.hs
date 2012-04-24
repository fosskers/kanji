module KanjiData where

import KanjiData.TenthQ
import KanjiData.NinthQ
import KanjiData.EighthQ
import KanjiData.SeventhQ
import KanjiData.SixthQ
import KanjiData.FifthQ
import KanjiData.FourthQ
import KanjiData.ThirdQ
import KanjiData.PreSecondQ
import KanjiData.SecondQ

allKanjiLists :: [String]
allKanjiLists = [tenthQ, ninthQ, eighthQ, seventhQ, sixthQ,
                 fifthQ, fourthQ, thirdQ, preSecondQ, secondQ]
