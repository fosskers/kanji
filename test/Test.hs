{-# LANGUAGE ImplicitParams #-}

module Main where

import           Data.Kanji
import           Data.Kanji.Types
import qualified Data.Map.Strict as M
import           Test.HUnit.Approx
import           Test.Tasty
import           Test.Tasty.HUnit

---

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "Unit Tests"
  [ testGroup "Analysis"
    [
      testCase "percentSpread totals 1.0" $ M.foldl' (+) 0 (percentSpread testKanji) @?~ 1.0
    , testCase "levelDist totals 1.0"     $ M.foldl' (+) 0 (levelDist testKanji) @?~ 1.0
    , testCase "levelDist == adultDen"    $ M.foldl' (+) 0 (levelDist testKanji) @?~ adultDen (levelDist testKanji)
    ]
  ]
  where ?epsilon = 0.001

testKanji :: [Kanji]
testKanji = map Kanji "本猫机鉛筆紙帽子包丁床天井壁光電気"
