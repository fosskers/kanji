{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns  #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Data.Kanji
import qualified Data.Text as T
import           Lucid
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Pages.Pages
import           Servant
import           Servant.HTML.Lucid
import           Text.Printf.TH
import qualified Data.Map.Lazy as M
import qualified Data.Set as S

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
server = (pure . analyse) :<|> pure (base home) :<|> serveDirectory "assets"

analyse :: [(T.Text,T.Text)] -> Html ()
analyse [] = base $ p_ "You didn't give any input!"
analyse ((_,""):_) = base $ p_ "You didn't give any input!"
analyse ((_,t):_) = base $ do
  p_ . toHtml $ [lt|Elementary Kanji: %.2f%%|] (100 * e)
  p_ . toHtml $ [lt|Kanji Density: %.2f%%|] (100 * d)
  p_ . toHtml $ [lt|Average Level: %.2f|] a
  h3_ "Level Densities"
  p_ . dist $ levelDist ks
  h3_ "Kanji per Level"
  p_ $ splits ks
  h3_ "Unknown Kanji"
  p_ . toHtml $ unknowns ks
    where ks = asKanji t
          e = elementaryKanjiDensity ks
          d = kanjiDensity (T.length t) ks
          a = averageLevel ks

-- | Convert "Level Distribution" results into a table.
dist :: [(Rank,Float)] -> Html ()
dist = table_ [class_ "table"] . mconcat . map f
  where f :: (Rank,Float) -> Html ()
        f (r,n) = tr_ $ do
          td_ . toHtml $ show r
          td_ . toHtml $ [lt|%.2f%%|] (100 * n)

-- | Show the `Kanji` that appeared in each Level as a table.
splits :: [Kanji] -> Html ()
splits = table_ [class_ "table"] . mconcat . map f . g
  where g = M.toList . S.foldl h M.empty . S.fromList
        h a k = maybe a (\l -> M.insertWith (++) (_rank l) [k] a) $ level k
        f :: (Rank,[Kanji]) -> Html ()
        f (r,ks) = tr_ $ do
          td_ . toHtml $ show r
          td_ . toHtml $ map _kanji ks

-- | Yield `Kanji` whose Level isn't known.
unknowns :: [Kanji] -> [Char]
unknowns = map _kanji . S.toList . S.filter (not . hasLevel) . S.fromList
