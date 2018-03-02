module Main where

import           Criterion.Main
import           Data.Foldable (fold)
import           Data.Kanji
import           Data.Kanji.Types
import qualified Data.Set as S

---

main :: IO ()
main = defaultMain
  [ bgroup "Analysis"
    [
      bench "testKanji - uniques" $ nf uniques testKanji
    , bench "bigKanji - uniques" $ nf uniques bigKanji
    , bench "level 一" $ nf level (Kanji '一')
    , bench "level 串" $ nf level (Kanji '串')
    ]
  ]

testKanji :: [Kanji]
testKanji = map Kanji "本猫机鉛筆紙帽子包丁床天井壁光電気棚箱靴家階段戸屋根地面"

bigKanji :: [Kanji]
bigKanji = S.toList $ fold allKanji
