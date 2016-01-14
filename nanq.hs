-- TODO: Move `Env` into a `Types` module, so that Data.Kanji can use it
-- via a `Reader` effect.

{-# LANGUAGE FlexibleContexts #-}

import           Control.Eff
import           Control.Eff.Reader.Lazy
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Kanji
import           Data.List (nub)
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (toLazyText)
import           Lens.Micro
import           Lens.Micro.Aeson
import           Options.Applicative
import           Text.Printf (printf)

---

data Flags = Flags [Operation] Language (Either FilePath T.Text)
  deriving (Eq)

data Operation = Unknowns | FromLevel | Density | Elementary | Distribution
  deriving (Eq)

data Language = Jap | Eng deriving (Show,Eq)

data Env = Env { _lang :: Language
               , _allKanji :: [Kanji]
               , _original :: T.Text } deriving Eq

-- | Long, Short, Help
lsh l s h = long l <> short s <> help h

flags :: Parser Flags
flags = Flags <$> operations <*> lang <*> (file <|> japanese)
  where lang = flag Jap Eng (lsh "japanese" 'j' "Output language is Japanese")
        file = Left <$> strOption (lsh "file" 'f' "Take input from a file")
        japanese = (Right . T.pack) <$> argument str (metavar "JAPANESE")

-- TODO: Better way to handle Maybes
operations :: Parser [Operation]
operations = (^.. each . _Just) <$> pairs
  where pairs = (,,,,) <$>
          flag (Just Unknowns) Nothing
          (lsh "unknowns" 'u' "Find Kanji whose Level couldn't be determined")
          <*> flag (Just FromLevel) Nothing
          (lsh "level" 'q' "Find Kanji from the requested Level")
          <*> flag (Just Density) Nothing
          (lsh "density" 'd' "Find how much of the input is made of Kanji")
          <*> flag (Just Elementary) Nothing
          (lsh "elementary" 'e' "Find how much of the Kanji is learnt in elementary school")
          <*> flag (Just Distribution) Nothing
          (lsh "leveldist" 'l' "Find the distribution of Kanji levels")

japQNames :: [(Rank,String)]
japQNames = zip rankNums ["10級","9級","8級","7級","6級","5級","4級",
                          "3級", "準2級","2級","準1級","1級"]

engQNames :: [(Rank,String)]
engQNames = zip rankNums ["Tenth Level","Ninth Level","Eighth Level",
                          "Seventh Level","Sixth Level","Fifth Level",
                          "Forth Level","Third Level", "Pre-Second Level",
                          "Second Level","Pre-First Level","First Level"]
                          
findAverageQ :: Language -> [Char] -> String
findAverageQ lang ks = 
  printf (getMsg lang ++ "%.2f") . averageLevel levels . asKanji $ ks
      where getMsg Eng = "Average Level: "
            getMsg Jap = "平均の級："

findQs :: Language -> [Char] -> [String]
findQs lang ks = map (nanQ lang) $ asKanji ks

nanQ :: Language -> Kanji -> String
nanQ lang k = maybe (bad lang) (good lang . _rank) $ level levels k
  where 
    good Eng n = (show k) ++ " is " ++ (articledQName n) ++ " Kanji."
    good Jap n = "「" ++ (show k) ++ "」は" ++ (jQName n) ++ "の漢字"
    bad Eng    = (show k) ++ " is not in any level."
    bad Jap    = "「" ++ (show k) ++ "」はどの級の漢字でもない"
    articledQName n = getArticle (eQName n) ++ " " ++ eQName n
    getArticle name = if head name `elem` "AEIOU" then "an" else "a"
    eQName n = getQName n engQNames
    jQName n = getQName n japQNames
    getQName n names = fromJust $ n `lookup` names

-- TODO: This should be two functions. One for finding the unknowns,
-- another for formatting output.
findUnknowns :: Language -> String -> [String]
findUnknowns lang ks = case unknowns of
                         [] -> [bad lang]
                         _  -> [good lang, map _kanji unknowns]
  where unknowns = nub . filter (not . hasLevel levels) . asKanji $ ks
        bad Eng  = "No Kanji of unknown Level found."
        bad Jap  = "級を明確にできない漢字は見つからなかった"
        good Eng = "The following Kanji of unknown Level were found:"
        good Jap = "級を明確にできなかった漢字は："

findDistribution :: Language -> String -> [String]
findDistribution lang ks =
  map (\(name,per) -> printf "%4s: %05.2f%%" name per) namePercentPairs
  where namePercentPairs     = map rawToPretty distributions
        distributions        = levelDist levels $ asKanji ks
        rawToPretty (qn,p)   = (getQName lang qn, p * 100)
        getQName Eng qn      = getName engQNames "Above Second Level" qn
        getQName Jap qn      = getName japQNames "2級以上" qn
        getName names msg qn = maybe msg id $ qn `lookup` names

howMuchIsKanji :: Language -> String -> String
howMuchIsKanji lang ks = printf "%s: %.2f%%" (getMsg lang) percent
  where percent = 100 * kanjiDensity ks
        getMsg Eng = "Kanji Density"
        getMsg Jap = "漢字率"

howMuchIsElementaryKanji :: Language -> String -> String
howMuchIsElementaryKanji lang ks = printf (getMsg lang) percent
  where percent    = 100 * elementaryKanjiDensity ks
        getMsg Eng = "Input Kanji is %.2f%% Elementary School Kanji."
        getMsg Jap = "入力した漢字は「%.2f%%」小学校で習う漢字。"

getAllFromLevel :: Rank -> [Char] -> [Kanji]
getAllFromLevel qn ks = maybe [] f $ levelFromRank levels qn
  where f q = nub . filter (isKanjiInLevel q) . asKanji $ ks

{-}
executeOpts :: ([Flag],Language,String) -> IO ()
executeOpts (flags,lang,input) =
  case flags of
    [Help]        -> putStr $ usageInfo usageMsg options
    [Average]     -> putStrLn $ findAverageQ lang input
    []            -> mapM_ putStrLn $ findQs lang input
    [Unknowns]    -> mapM_ putStrLn $ findUnknowns lang input
    [LevelDist]   -> mapM_ putStrLn $ findDistribution lang input
    [KDensity]    -> putStrLn $ howMuchIsKanji lang input
    [Elementary]  -> putStrLn $ howMuchIsElementaryKanji lang input
    [Text]        -> mapM_ execAll [KDensity,Elementary,LevelDist]
    [AllFromQ qn] -> putStrLn . map _kanji $ getAllFromLevel qn input
    _             -> argError "Conflicting flags given."
    where execAll flag = executeOpts ([flag],lang,input) >> putStrLn ""
-}

-- | All operations return JSON, to be aggregated into a master Object.
execOp :: Member (Reader Env) r => Operation -> Eff r Value
execOp Unknowns = undefined
execOp FromLevel = undefined
execOp Density = undefined
execOp Elementary = undefined
execOp Distribution = undefined

output :: Value -> IO ()
output = TIO.putStrLn . toStrict . toLazyText . encodePrettyToTextBuilder

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
