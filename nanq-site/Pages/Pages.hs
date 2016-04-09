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
    body_ body

-- | The Home Page, accessible through the `/` endpoint.
home :: Html ()
home = containerFluid_ $ do
  row_ $ do
    col6_ [class_ "col-md-offset-3"] $ h1_ "NanQ - Analyse Japanese Text"

