{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Eff
import           Control.Eff.Reader.Lazy
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.HashMap.Strict as HMS
import           Data.Kanji
import qualified Data.Map.Lazy as M
import           Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Text as TS
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Vector as V
import           Lens.Micro
import           Lens.Micro.Aeson
import           Options.Applicative
import           Text.Printf (printf)

---

data Flags = Flags [Operation] Language (Either FilePath T.Text)
  deriving (Eq)

data Operation = Unknowns | Density | Elementary
  | Distribution | Average | Splits deriving (Eq)

data Language = Jap | Eng deriving (Show,Eq)

data Env = Env { _lang :: Language
               , _allKs :: [Kanji]
               , _original :: T.Text } deriving Eq

-- | Long, Short, Help
lsh l s h = long l <> short s <> help h

flags :: Parser Flags
flags = Flags <$> operations <*> lang <*> (file <|> japanese)
  where lang = flag Jap Eng (lsh "japanese" 'j' "Output language is Japanese")
        file = Left <$> strOption (lsh "file" 'f' "Take input from a file")
        japanese = (Right . T.pack) <$> argument str (metavar "JAPANESE")

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

japQNames :: [(Rank,String)]
japQNames = zip [Ten ..] ["10級","9級","8級","7級","6級","5級","4級",
                          "3級", "準2級","2級","準1級","1級"]

engQNames :: [(Rank,String)]
engQNames = zip [Ten ..] ["Tenth Level","Ninth Level","Eighth Level",
                          "Seventh Level","Sixth Level","Fifth Level",
                          "Forth Level","Third Level", "Pre-Second Level",
                          "Second Level","Pre-First Level","First Level"]
                          
averageLev :: Member (Reader Env) r => Eff r Value
averageLev = do
  average <- averageLevel levels <$> reader _allKs
  pure $ object [ "average" .= average ]

splits :: Member (Reader Env) r => Eff r Value
splits = do
  ks <- M.toList . S.foldl f m . S.fromList <$> reader _allKs
  pure $ object [ "levelSplit" .= object (map g $ filter h ks) ]
    where m = M.fromList $ map (,[]) [Ten .. Two]
          f a k = maybe a (\l -> M.adjust (k :) (_rank l) a) $ level levels k
          h (_,[]) = False  -- Exclude Levels for which none were found.
          h _ = True
          g (r,ks) = TS.pack (show r) .= map _kanji ks

unknowns :: Member (Reader Env) r => Eff r Value
unknowns = do
  ks <- S.filter (not . hasLevel levels) . S.fromList <$> reader _allKs
  pure $ object [ "unknowns" .= map _kanji (S.toList ks) ]

distribution :: Language -> String -> [String]
distribution lang ks =
  map (\(name,per) -> printf "%4s: %05.2f%%" name per) namePercentPairs
  where namePercentPairs     = map rawToPretty distributions
        distributions        = levelDist levels $ asKanji ks
        rawToPretty (qn,p)   = (getQName lang qn, p * 100)
        getQName Eng qn      = getName engQNames "Above Second Level" qn
        getQName Jap qn      = getName japQNames "2級以上" qn
        getName names msg qn = maybe msg id $ qn `lookup` names

density :: Member (Reader Env) r => Eff r Value
density = do
  d <- kanjiDensity <$> reader _original <*> reader _allKs
  pure $ object [ "density" .= d ]

elementaryDensity :: Member (Reader Env) r => Eff r Value
elementaryDensity = do
  d <- elementaryKanjiDensity <$> reader _allKs
  pure $ object [ "elementary" .= d ]

-- | All operations return JSON, to be aggregated into a master Object.
execOp :: Member (Reader Env) r => Operation -> Eff r Value
execOp Unknowns = unknowns
execOp Density = density
execOp Elementary = elementaryDensity
execOp Distribution = undefined
execOp Average = averageLev
execOp Splits = splits

output :: Value -> IO ()
output = TIO.putStrLn . toLazyText . encodePrettyToTextBuilder

-- | Dispatch on each `Operation` given. Aggregates the resulting JSON.
work :: (Env, [Operation]) -> Value
work (e,os) = Object $ vals ^. each . _Object
  where vals = run $ runReader (mapM execOp os) e

env :: Flags -> IO (Env, [Operation])
env (Flags os l (Right t)) = pure (Env l (asKanji t) t, os)
env (Flags os l (Left f)) = do
  t <- TIO.readFile f
  pure (Env l (asKanji t) t, os)

main :: IO ()
main = execParser opts >>= env >>= output . work
  where opts = info (helper <*> flags)
          (fullDesc <> header "nanq - Kanji analysis of Japanese text")
