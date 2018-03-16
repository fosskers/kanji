{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Main ( main ) where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Kanji
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Text.Lazy.Builder (toLazyText)
import           Lens.Micro
import           Lens.Micro.Aeson
import           Lens.Micro.Platform ()
import           Options.Applicative
import           Protolude hiding (to)

---

data Flags = Flags [Operation] (Either FilePath Text) deriving (Eq)

data Operation = Density | Elementary | Distribution | Splits deriving (Eq)

data Env = Env { _allKs :: [Kanji]
               , _original :: Text } deriving Eq

-- | Long, Short, Help
lsh l s h = long l <> short s <> help h

flags :: Parser Flags
flags = Flags <$> operations <*> (file <|> japanese)
  where file = Left <$> strOption (lsh "file" 'f' "Take input from a file")
        japanese = Right <$> argument str (metavar "JAPANESE")

operations :: Parser [Operation]
operations = catMaybes <$> ops
  where ops = traverse optional
          [ flag' Density $
            lsh "density" 'd' "Find how much of the input is made of Kanji"
          , flag' Elementary $
            lsh "elementary" 'e' "Find density of Kanji learnt in elementary school"
          , flag' Distribution $
            lsh "leveldist" 'l' "Find the distribution of Kanji levels"
          , flag' Splits $
            lsh "splits" 's' "Show which Level each Kanji belongs to" ]

-- | Shortcut for singleton objects
ob :: ToJSON v => Text -> v -> Value
ob k v = object [ k .= v ]

splits :: Reader Env Value
splits = ob "levelSplit" . S.foldl' f mempty . S.fromList <$> asks _allKs
  where f a k = (\l -> M.insertWith (++) l [k] a) $ level k

distribution :: Reader Env Value
distribution = ob "distributions" . levelDist <$> asks _allKs

density :: Reader Env Value
density = do
  d <- kanjiDensity <$> asks (T.length . _original) <*> asks _allKs
  pure $ ob "density" d

elementaryDensity :: Reader Env Value
elementaryDensity = ob "elementary" . elementaryDen . levelDist <$> asks _allKs

-- | All operations return JSON, to be aggregated into a master Object.
execOp :: Operation -> Reader Env Value
execOp Density = density
execOp Elementary = elementaryDensity
execOp Distribution = distribution
execOp Splits = splits

output :: Value -> IO ()
output = putLText . toLazyText . encodePrettyToTextBuilder

-- | Dispatch on each `Operation` given. Aggregates the resulting JSON.
work :: (Env, [Operation]) -> Value
work (e, os) = Object $ vals ^. each . _Object
  where vals = runReader (traverse execOp os) e

env :: Flags -> IO (Env, [Operation])
env (Flags os inp) = case inp of
  Right t -> pure (e t, os)
  Left  f -> (, os) . e <$> TIO.readFile f
  where e t = Env (t ^.. each . to kanji . _Just) t

main :: IO ()
main = execParser opts >>= env >>= output . work
  where opts = info (helper <*> flags)
          (fullDesc <> header "nanq - Kanji analysis of Japanese text")
