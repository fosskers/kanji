{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Eff
import           Control.Eff.Reader.Lazy
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Kanji
import qualified Data.Map.Lazy as M
import           Data.Monoid ((<>))
import qualified Data.Set as S
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.IO as TIO
import           Lens.Micro
import           Lens.Micro.Aeson
import           Options.Applicative

---

data Flags = Flags [Operation] (Either FilePath TL.Text) deriving (Eq)

data Operation = Unknowns | Density | Elementary
  | Distribution | Average | Splits deriving (Eq)

data Env = Env { _allKs :: [Kanji]
               , _original :: TL.Text } deriving Eq

-- | Long, Short, Help
lsh l s h = long l <> short s <> help h

flags :: Parser Flags
flags = Flags <$> operations <*> (file <|> japanese)
  where file = Left <$> strOption (lsh "file" 'f' "Take input from a file")
        japanese = (Right . TL.pack) <$> argument str (metavar "JAPANESE")

operations :: Parser [Operation]
operations = (^.. each . _Just) <$> ops
  where ops = sequenceA $ map optional
          [ flag' Unknowns $
            lsh "unknowns" 'u' "Find Kanji whose Level couldn't be determined"
          , flag' Density $
            lsh "density" 'd' "Find how much of the input is made of Kanji"
          , flag' Elementary $
            lsh "elementary" 'e' "Find density of Kanji learnt in elementary school"
          , flag' Distribution $
            lsh "leveldist" 'l' "Find the distribution of Kanji levels"
          , flag' Average $
            lsh "average" 'a' "Find the average Level of all Kanji present"
          , flag' Splits $
            lsh "splits" 's' "Show which Level each Kanji belongs to" ]

-- | Shortcut for singleton objects
ob :: ToJSON v => TS.Text -> v -> Value
ob k v = object [ k .= v ]

averageLev :: Member (Reader Env) r => Eff r Value
averageLev = ob "average" . averageLevel <$> reader _allKs

splits :: Member (Reader Env) r => Eff r Value
splits = do
  ks <- map g . M.toList . S.foldl f M.empty . S.fromList <$> reader _allKs
  pure $ object [ "levelSplit" .= object ks ]
    where f a k = maybe a (\l -> M.insertWith (++) (_rank l) [k] a) $ level k
          g (r,ks) = TS.pack (show r) .= map _kanji ks

unknowns :: Member (Reader Env) r => Eff r Value
unknowns = do
  ks <- S.filter (not . hasLevel) . S.fromList <$> reader _allKs
  pure $ object [ "unknowns" .= map _kanji (S.toList ks) ]

distribution :: Member (Reader Env) r => Eff r Value
distribution = ob "distributions" . object . map f . levelDist <$> reader _allKs
    where f (r,v) = TS.pack (show r) .= v

density :: Member (Reader Env) r => Eff r Value
density = do
  d <- kanjiDensity <$> reader (fromIntegral . TL.length . _original) <*> reader _allKs
  pure $ object [ "density" .= d ]

elementaryDensity :: Member (Reader Env) r => Eff r Value
elementaryDensity = ob "elementary" . elementaryDen . levelDist <$> reader _allKs

-- | All operations return JSON, to be aggregated into a master Object.
execOp :: Member (Reader Env) r => Operation -> Eff r Value
execOp Unknowns = unknowns
execOp Density = density
execOp Elementary = elementaryDensity
execOp Distribution = distribution
execOp Average = averageLev
execOp Splits = splits

output :: Value -> IO ()
output = TIO.putStrLn . toLazyText . encodePrettyToTextBuilder

-- | Dispatch on each `Operation` given. Aggregates the resulting JSON.
work :: (Env, [Operation]) -> Value
work (e,os) = Object $ vals ^. each . _Object
  where vals = run $ runReader (mapM execOp os) e

env :: Flags -> IO (Env, [Operation])
env (Flags os (Right t)) = pure (Env (asKanji t) t, os)
env (Flags os (Left f)) = do
  t <- TIO.readFile f
  pure (Env (asKanji t) t, os)

main :: IO ()
main = execParser opts >>= env >>= output . work
  where opts = info (helper <*> flags)
          (fullDesc <> header "nanq - Kanji analysis of Japanese text")
