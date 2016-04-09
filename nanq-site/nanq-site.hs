{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Data.Aeson
import Data.List
import GHC.Generics
import Lucid
import Network.HTTP.Media
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid

---

type PersonAPI = "persons" :> Get '[JSON, HTML] [Person]

data Person = Person { firstName :: String, lastName :: String } deriving Generic

instance ToJSON Person

instance ToHtml Person where
  toHtml person = tr_ $ do
    td_ (toHtml $ firstName person)
    td_ (toHtml $ lastName person)

  toHtmlRaw = toHtml

instance ToHtml [Person] where
  toHtml ps = table_ $ do
    tr_ $ th_ "first name" *> th_ "last name"
    foldMap toHtml ps

  toHtmlRaw = toHtml

persons :: [Person]
persons = [ Person "Colin" "Woodbury"
          , Person "Jack" "Catworthy" ]

personAPI :: Proxy PersonAPI
personAPI = Proxy

server :: Server PersonAPI
server = pure persons

app :: Application
app = serve personAPI server

main :: IO ()
main = run 8081 app
