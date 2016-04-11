{-# LANGUAGE OverloadedStrings #-}

module Pages.Pages where

import Lucid
import Pages.Bootstrap
       
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
            center_ $ a_ [href_ "https://github.com/fosskers/nanq"] "Github"

-- | The Home Page, accessible through the `/` endpoint.
home :: Html ()
home = row_ $ do
  col4_ [class_ "col-md-offset-1"] explanation
  col6_ form --[class_ "col-md-offset-1"] form

form :: Html ()
form = form_ [action_ "/analyse", method_ "POST"] $ do
  div_ [class_ "input"] $ do
    label_ [for_ "japText"] "Japanese Text"
    textarea_ [ id_ "japText"
              , class_ "form-control"
              , name_ "japText"
              , rows_ "15"
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
