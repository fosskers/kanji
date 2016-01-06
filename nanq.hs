import Data.Kanji
import Data.List (delete, nub)
import Data.Maybe (fromJust)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (hGetContents, stdin)
import Text.Printf (printf)

---

data Language = Jap | Eng deriving (Show,Eq)

data Flag = FileInput | PipeInput | Help      | Average    |
            Unknowns  | LevelDist | KDensity  | Elementary |
            Text      | JapOutput | AllFromQ Rank deriving (Show,Eq)

options :: [OptDescr Flag]
options = [ Option ['f'] ["file"]       (NoArg FileInput)  fDesc
          , Option ['p'] ["pipe"]       (NoArg PipeInput)  pDesc
          , Option ['h'] ["help"]       (NoArg Help)       hDesc
          , Option ['a'] ["average"]    (NoArg Average)    aDesc
          , Option ['u'] ["unknowns"]   (NoArg Unknowns)   uDesc
          , Option ['d'] ["leveldist"]  (NoArg LevelDist)  lDesc
          , Option ['k'] ["density"]    (NoArg KDensity)   kDesc
          , Option ['e'] ["elementary"] (NoArg Elementary) eDesc
          , Option ['t'] ["text"]       (NoArg Text)       tDesc
          , Option ['j'] ["japanese"]   (NoArg JapOutput)  jDesc 
          , Option ['q'] ["fromq"]
            (ReqArg (\s -> AllFromQ (read s :: Rank)) "Rank") qDesc
          ]
    where fDesc = "Takes input from a given file." ++
                  "\n入力は指定のファイルから"
          pDesc = "Takes input from a pipe (stdin)." ++
                  "\n入力はpipe (つまりstdin)から"
          hDesc = "Shows this help message." ++
                  "\nこのメッセージから出力"
          aDesc = "Find the average level of some given line of Japanese." ++
                  "\n日本語の文の平均的な級を求める"
          uDesc = "Report Kanji whose Level could not be determined." ++
                  "\n級を明確にできなかった漢字を報告"
          lDesc = "Find the % distribution of levels in given Japanese." ++
                  "\nどの級の漢字がどれ程出ているか、パーセントで出力"
          kDesc = "Determines how much of the input is made of Kanji." ++
                  "\n入力は何パーセント漢字でできているか出力"
          eDesc = "Determines how much of the input is made of Kanji" ++
                  "\nlearnt in Elementary School in Japan." ++
                  "\n入力は何パーセント小学校で習う漢字でできているか出力"
          tDesc = "Applies -k -e and -d all at once to analyse some text." ++
                  "\n-k、-e、-dを連続に使って入力を分析"
          jDesc = "Output is given in Japanese instead of English." ++
                  "\n出力の際は日本語"
          qDesc = "Filters out all but Kanji from the requested Level." ++
                  "\n指定された級の漢字だけ出力"          
          
usageMsg :: String
usageMsg = "Usage : nanq [OPTION] (kanji / file)"

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

findUnknowns :: Language -> String -> [String]
findUnknowns lang ks = case unknowns of
                         [] -> [bad lang]
                         _  -> good lang : map show unknowns
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

parseOpts :: [String] -> IO ([Flag],[String])
parseOpts args = case getOpt Permute options args of
                   (opts,nonopts,[]) -> return (opts,nonopts)
                   (_,_,_)           -> argError "Bad flag used."

-- Determine input source and output language.
cleanOpts :: ([Flag],[String]) -> IO ([Flag],Language,String)
cleanOpts (opts,nonopts) = clean opts nonopts Eng  -- English by default.
    where clean opts nonopts lang 
            | JapOutput `elem` opts = clean (without JapOutput) nonopts Jap
            | Help `elem` opts      = return ([Help],lang,"")
            | PipeInput `elem` opts = do input <- hGetContents stdin
                                         return (without PipeInput,lang,input)
            | null nonopts          = argError "No Kanji / file given!"
            | FileInput `elem` opts = do input <- readFile $ head nonopts
                                         return (without FileInput,lang,input)
            | otherwise             = return (opts,lang,head nonopts)
            where without x = delete x opts

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
    [AllFromQ qn] -> mapM_ print $ getAllFromLevel qn input
    _             -> argError "Conflicting flags given."
    where execAll flag = executeOpts ([flag],lang,input) >> putStrLn ""

argError :: String -> a
argError msg = error $ usageInfo (msg ++ "\n" ++ usageMsg) options

main :: IO ()
main = do
  args <- getArgs
  opts <- parseOpts args
  cleanedOpts <- cleanOpts opts
  executeOpts cleanedOpts
