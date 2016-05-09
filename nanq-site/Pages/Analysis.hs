{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Pages.Analysis where

import           Data.Kanji
import           Data.List (intersperse)
import qualified Data.Map.Lazy as M
import           Data.Monoid ((<>))
import qualified Data.Set as S
import qualified Data.Text as T
import           Lucid
import           Pages.Bootstrap
import           Text.Printf.TH

---

analysis :: [(T.Text,T.Text)] -> Html ()
analysis [] = analyse ""
analysis ((_,""):_) = analyse ""
analysis ((_,t):_) = analyse t

analyse :: T.Text -> Html ()
analyse "" = center_ "You didn't give any input."
analyse t = do
  script_ [src_ "/assets/jquery.js", charset_ "utf-8"] T.empty
  script_ [src_ "/assets/highcharts.js", charset_ "utf-8"] T.empty
  row_ $
    col10_ [class_ "col-md-offset-1"] $ do
      p_ $ do
        toHtml $ [lt|Kanji Density: %.2f%%|] (100 * d)
        i_ $ small_ "   ...How much of the source text is Kanji"
      p_ $ do
        toHtml $ [lt|Elementary Kanji: %.2f%%|] (100 * e)
        i_ $ small_ "   ...How much of the Kanji is learned in Elementary School"
      p_ $ do
        toHtml $ [lt|Average Level: %.2f|] a
        i_ $ small_ "   ...The average Level of all Kanji in the text"
      h3_ "Level Densities・級の密度"
      p_ $ dist ld
      div_ [id_ "nanqchart"] ""
      pie ld
      h3_ "Kanji per Level・級別漢字"
      p_ $ splits ks
      h3_ "Unknown Kanji・未確定"
      center_ . h2_ . toHtml . f . intersperse ' ' $ unknowns ks
    where ks = asKanji t
          ld = levelDist ks
          e = elementaryDen ld
          d = kanjiDensity (T.length t) ks
          a = averageLevel ks
          f "" = "None・無し"
          f x = x

pie :: [(Rank,Float)] -> Html ()
pie rf = script_ $ [st|

$(function () {
  $('#nanqchart').highcharts({
    chart: {
      plotBackgroundColor: null,
      plotBorderWidth: null,
      plotShadow: false,
      type: 'pie'
    },
    title: { text: 'Level Densities' },
    tooltip: {
      pointFormat: '{series.name}: <b>{point.percentage:.1f}%%</b>'
    },
    plotOptions: {
      pie: {
        allowPointSelect: true,
        cursor: 'pointer',
        dataLabels: {
          enabled: true,
          format: '<b>{point.name}</b>: {point.percentage:.1f} %%',
          style: {
            color: (Highcharts.theme && Highcharts.theme.contrastTextColor) || 'black'
          }
        }
      }
    },
    series: [{
      name: 'Levels',
      colorByPoint: true,
      data: [%s]
    }]
  });
});

|] datums

  where datums = mconcat . intersperse "," $ map f rf
        f (r,n) = [st|{name: '%s', y: %f}|] (show r) n

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
