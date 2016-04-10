{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns  #-}

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
import Pages.Pages
import qualified Data.Text as T

---

--"persons" :> Get '[JSON, HTML] [Person]
type API = "analyse" :> ReqBody '[FormUrlEncoded] [(T.Text,T.Text)] :> Post '[HTML] (Html ())
           :<|> Get '[HTML] (Html ())
           :<|> "assets" :> Raw

--data Jap = Jap { japText :: TL.Text } deriving Generic

--instance FromJSON Jap

{-}
data Person = Person { firstName :: String
                     , lastName :: String } deriving Generic

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
-}

api :: Proxy API
api = Proxy

server :: Server API
server = (pure . foo) :<|> pure (base home) :<|> serveDirectory "assets"

app :: Application
app = serve api server

main :: IO ()
main = run 8081 app

foo :: [(T.Text,T.Text)] -> Html ()
foo [] = base $ p_ "You didn't give any input!"
foo ((_,""):_) = base $ p_ "You didn't give any input!"
foo ((_,t):_) = base $ p_ (toHtml t)
