{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

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
server = pure . base . analysis
  :<|> pure (base home)
  :<|> serveDirectory "assets"
