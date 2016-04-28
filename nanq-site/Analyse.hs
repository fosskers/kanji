{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Analyse where

import           Data.Kanji
import qualified Data.Map.Lazy as M
import           Data.Monoid ((<>))
import qualified Data.Set as S
import           Lucid
import           Text.Printf.TH

---

-- | Convert "Level Distribution" results into a table.
dist :: [(Rank,Float)] -> Html ()
dist = table_ [class_ "table table-striped"] . mconcat . map f
  where f :: (Rank,Float) -> Html ()
        f (r,n) = tr_ $ do
          td_ . i_ . toHtml $ show r
          td_ . toHtml $ [lt|%.2f%%|] (100 * n)

-- | Show the `Kanji` that appeared in each Level as a table.
splits :: [Kanji] -> Html ()
splits = table_ [class_ "table table-striped"] . (hs <>) . foldMap f . g
  where g = M.toList . S.foldl h M.empty . S.fromList
        h a k = maybe a (\l -> M.insertWith (++) (_rank l) [k] a) $ level k
        f :: (Rank,[Kanji]) -> Html ()
        f (r,ks) = tr_ $ do
          td_ . i_ . toHtml $ show r
          td_ . toHtml $ map _kanji ks
          td_ . toHtml $ [lt|%d / %d|] (length ks)
            (maybe 0 (S.size . _allKanji) $ levelFromRank r)
        hs = tr_ $
          th_ "Level" *> th_ "Unique Kanji" *> th_ "Fraction of Level Total"

-- | Yield `Kanji` whose Level isn't known.
unknowns :: [Kanji] -> [Char]
unknowns = map _kanji . S.toList . S.filter (not . hasLevel) . S.fromList
