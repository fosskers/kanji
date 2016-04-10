{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns  #-}

module Main where

import qualified Data.Text as T
import           Lucid
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Pages.Pages
import           Servant
import           Servant.HTML.Lucid

---

-- | All endpoints for our server.
type API = "analyse" :> ReqBody '[FormUrlEncoded] [(T.Text,T.Text)]
                     :> Post '[HTML] (Html ())
           :<|> Get '[HTML] (Html ())
           :<|> "assets" :> Raw

main :: IO ()
main = run 8081 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = (pure . formInput) :<|> pure (base home) :<|> serveDirectory "assets"

formInput :: [(T.Text,T.Text)] -> Html ()
formInput [] = base $ p_ "You didn't give any input!"
formInput ((_,""):_) = base $ p_ "You didn't give any input!"
formInput ((_,t):_) = base $ p_ (toHtml t)
