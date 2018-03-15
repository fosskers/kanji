{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main ( main ) where

import           Data.Aeson
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
    , testCase "levelDist == adultDen"    $ M.foldl' (+) 0 (levelDist testKanji) @?~ adultDen (levelDist testKanji)
    ]
  , testGroup "JSON"
    [
      testCase "Empty String should fail to parse"
      $ assertBool "An empty String somehow parsed" . isError $ fromJSON @Kanji (String "")
    , testCase "String w/ multiple Kanji should fail"
      $ assertBool "A String of multiple Kanji somehow parsed" . isError $ fromJSON @Kanji (String "泥棒猫")
    , testCase "String w/ one Kanji should succeed"   $ fromJSON (String "猫") @?= Success (Kanji '猫')
    , testCase "Isomorphism" $ fromJSON (toJSON $ Kanji '猫') @?= Success (Kanji '猫')
    , testCase "newtype generic encoding" $ toJSON (Kanji '猫') @?= String "猫"
    ]
  ]
  where ?epsilon = 0.001

testKanji :: [Kanji]
testKanji = map Kanji "本猫机鉛筆紙帽子包丁床天井壁光電気棚箱靴家階段戸屋根地面糞"

isError :: Result a -> Bool
isError (Error _) = True
isError _ = False
