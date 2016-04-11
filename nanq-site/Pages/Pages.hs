{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Pages.Pages where

import           Analyse
import           Data.Kanji
import           Data.List (intersperse)
import qualified Data.Text as T
import           Lucid
import           Pages.Bootstrap
import           Text.Printf.TH

---

-- | The basic template for all pages.
base :: Html () -> Html ()
base body = do
  doctype_
  html_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      title_ "NanQ - Analyse Japanese Text"
      link_ [ href_ "/assets/bootstrap.min.css"
            , rel_ "stylesheet"
            , media_ "all"
            , type_ "text/css" ]
    body_ $ do
      containerFluid_ $ do
        row_ $
          col6_ [class_ "col-md-offset-3"] $
            div_ [class_ "header clearfix"] $ do
              h3_ [class_ "text-muted"] "NanQ - Analyse Japanese Text"
              hr_ []
        body
        row_ $
          col6_ [class_ "col-md-offset-3"] $ do
            hr_ []
            center_ $ do
              a_ [href_ "https://github.com/fosskers/nanq"] "Github"
              "・"
              a_ [href_ "mailto:colingw@gmail.com"] "Contact"

-- | The Home Page, accessible through the `/` endpoint.
home :: Html ()
home = row_ $ do
  col4_ [class_ "col-md-offset-1"] explanation
  col6_ form

form :: Html ()
form = form_ [action_ "/analyse", method_ "POST"] $ do
  div_ [class_ "input"] $ do
    label_ [for_ "japText"] "Japanese Text"
    textarea_ [ id_ "japText"
              , class_ "form-control"
              , name_ "japText"
              , rows_ "15"
              , placeholder_ "Paste your text here."
              ] ""
    center_ [style_ "padding-top:15px;"] $
      button_ [ class_ "btn btn-primary btn-lg", type_ "submit" ] "Analyse"

explanation :: Html ()
explanation = div_ [class_ "jumbotron"] $ do
  p_ . toHtml $ unwords [ "Are you a learner or native speaker of Japanese?"
                        , "You can use this website to analyse Japanese text"
                        , "for its Kanji difficulty."
                        ]
  p_ $ mconcat [ "日本語のネイティブも勉強者も、日本語の文章の漢字難易度を"
               , "分析するためにこのサイトを無料に利用できます。" ]

analysis :: [(T.Text,T.Text)] -> Html ()
analysis [] = analyse ""
analysis ((_,""):_) = analyse ""
analysis ((_,t):_) = analyse t

-- <script src="https://d3js.org/d3.v3.min.js" charset="utf-8"></script>
analyse :: T.Text -> Html ()
analyse "" = center_ "You didn't give any input."
analyse t = do
  script_ [src_ "assets/d3.min.js", charset_ "utf-8"] T.empty
  row_ $
    col10_ [class_ "col-md-offset-1"] $ do
      p_ . toHtml $ [lt|Elementary Kanji: %.2f%%|] (100 * e)
      p_ . toHtml $ [lt|Kanji Density: %.2f%%|] (100 * d)
      p_ . toHtml $ [lt|Average Level: %.2f|] a
      h3_ "Level Densities"
      p_ . dist $ levelDist ks
      h3_ "Kanji per Level"
      p_ $ splits ks
      h3_ "Unknown Kanji"
      center_ . h2_ . toHtml . intersperse ' ' $ unknowns ks
    where ks = asKanji t
          e = elementaryKanjiDensity ks
          d = kanjiDensity (T.length t) ks
          a = averageLevel ks
