import System.IO (hGetContents, stdin)
import System.Environment (getArgs)
import Data.List (delete, nub)
import Data.Maybe (fromJust)
import System.Console.GetOpt
import Text.Printf (printf)
import KanjiQ

data Language = Jap | Eng deriving (Show,Eq)

data Flag = FileInput | PipeInput | Help      | Average    |
            Unknowns  | LevelDist | KDensity  | Elementary |
            JapOutput | AllFromQ QNum deriving (Show,Eq)            

options :: [OptDescr Flag]
options = [ Option ['f'] ["file"]       (NoArg FileInput)  fDesc
          , Option ['p'] ["pipe"]       (NoArg PipeInput)  pDesc
          , Option ['h'] ["help"]       (NoArg Help)       hDesc
          , Option ['a'] ["average"]    (NoArg Average)    aDesc
          , Option ['u'] ["unknowns"]   (NoArg Unknowns)   uDesc
          , Option ['d'] ["leveldist"]  (NoArg LevelDist)  lDesc
          , Option ['k'] ["density"]    (NoArg KDensity)   kDesc
          , Option ['e'] ["elementary"] (NoArg Elementary) eDesc
          , Option ['j'] ["japanese"]   (NoArg JapOutput)  jDesc 
          , Option ['q'] ["fromq"]
            (ReqArg (\s -> AllFromQ (read s :: QNum)) "QNum") qDesc
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
          jDesc = "Output is given in Japanese instead of English." ++
                  "\n出力の際は日本語"
          qDesc = "Filters out all but Kanji in the given Level." ++
                  "\n指定された級の漢字だけ出力"          
          
usageMsg :: String
usageMsg = "Usage : nanq [OPTION] (kanji / file)"

japQNames :: [(QNum,String)]
japQNames = zip qNumbers ["10級","9級","8級","7級","6級","5級","4級",
                          "3級", "準2級","2級","準1級","1級"]

engQNames :: [(QNum,String)]
engQNames = zip qNumbers ["Tenth Level","Ninth Level","Eighth Level",
                          "Seventh Level","Sixth Level","Fifth Level",
                          "Forth Level","Third Level", "Pre-Second Level",
                          "Second Level","Pre-First Level","First Level"]
                          
main = do
  args <- getArgs
  opts <- processOpts args
  cleanedOpts <- cleanOpts opts
  case cleanedOpts of
    ([Help],_,_)              -> putStr $ usageInfo usageMsg options
    ([Average],   lang,input) -> findAverageQ lang input
    ([],          lang,input) -> findQs lang input
    ([Unknowns],  lang,input) -> findUnknowns lang input
    ([LevelDist], lang,input) -> findPercentDistribution lang input
    ([KDensity],  lang,input) -> howMuchIsKanji lang input
    ([Elementary],lang,input) -> howMuchIsElementaryKanji lang input
    ([AllFromQ qn],_,  input) -> getAllFromQ qn input
    (flagsInConflict,_,_)     -> argError "Conflicting flags given."

processOpts :: [String] -> IO ([Flag],[String])
processOpts args =
    case getOpt Permute options args of
      (opts,nonopts,[]) -> return (opts,nonopts)
      (_,_,errors)      -> argError "Bad flag used."

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

argError :: String -> a
argError msg = error $ usageInfo (msg ++ "\n" ++ usageMsg) options

findAverageQ :: Language -> String -> IO ()
findAverageQ lang ks = do
  qs <- allQs
  printf (getMsg lang ++ "%.2f\n") . averageQ qs . allToKanji $ ks
      where getMsg Eng = "Average Level: "
            getMsg Jap = "平均の級："

findQs :: Language -> String -> IO ()
findQs lang ks = do
  results <- mapM (nanQ (pass lang) (fail lang)) $ allToKanji ks
  mapM_ putStrLn results
    where
      pass Eng = \k n -> (show k) ++ " is a " ++ (eQName n) ++ " Kanji."
      pass Jap = \k n -> "「" ++ (show k) ++ "」は" ++ (jQName n) ++ "の漢字"
      fail Eng = \k -> (show k) ++ " is not in any level."
      fail Jap = \k -> "「" ++ (show k) ++ "」はどの級の漢字でもない"
      eQName n = getQName n engQNames
      jQName n = getQName n japQNames
      getQName n names = fromJust $ n `lookup` names

nanQ :: (Kanji -> Double -> String) -> (Kanji -> String) -> Kanji -> IO String
nanQ pass fail k = do
  qs <- allQs
  case whatQ qs k of
    Just qNum -> return $ pass k qNum
    Nothing   -> return $ fail k

findUnknowns :: Language -> String -> IO ()
findUnknowns lang ks = do
  qs <- allQs
  let unknowns = nub . filter (isUnknown qs) . allToKanji $ ks
  case unknowns of
    [] -> putStrLn (fail lang)
    _  -> putStrLn (pass lang) >> mapM_ print unknowns          
    where fail Eng = "No Kanji of unknown Level found."
          fail Jap = "級を明確にできない漢字は見つからなかった"
          pass Eng = "The following Kanji of unknown Level were found:"
          pass Jap = "級を明確にできなかった漢字は："

findPercentDistribution :: Language -> String -> IO ()
findPercentDistribution lang ks = do
  qs <- allQs
  let distributions    = qDistribution qs $ allToKanji ks
      namePercentPairs = map rawToPretty distributions
  mapM_ (\(name,per) -> printf "%4s: %05.2f%%\n" name per) namePercentPairs
      where rawToPretty (qn,p)   = (getQName lang qn, p * 100)
            getQName Eng qn      = getName engQNames "Above Second Level" qn
            getQName Jap qn      = getName japQNames "2級以上" qn
            getName names msg qn = case qn `lookup` names of
                                     Just name -> name
                                     Nothing   -> msg

howMuchIsKanji :: Language -> String -> IO ()
howMuchIsKanji lang ks = printf "%s: %.2f%%\n" msg percent
    where percent = 100 * (length' asKanji / length' ks :: Float)
          length' = fromIntegral . length
          asKanji = allToKanji ks
          msg     = getMsg lang
          getMsg Eng = "Kanji Density"
          getMsg Jap = "漢字率"

howMuchIsElementaryKanji :: Language -> String -> IO ()
howMuchIsElementaryKanji lang ks = do
  qs <- allQs
  let distributions = qDistribution qs $ allToKanji ks
      elementaryQs  = filter (\(qn,_) -> qn `elem` [5..10]) distributions
      percentSum    = 100 * foldl (\acc (_,p) -> acc + p) 0 elementaryQs
  printf (getMsg lang) percentSum
      where getMsg Eng = "Input Kanji is %.2f%% Elementary School Kanji.\n"
            getMsg Jap = "入力した漢字は「%.2f%%」小学校で習う漢字。\n"

getAllFromQ :: QNum -> String -> IO ()
getAllFromQ qn ks = do
  qs <- allQs
  mapM_ print $ case getQ qs qn of
                  Just q  -> nub . filter (isKanjiInQ q) . allToKanji $ ks
                  Nothing -> []
  
allToKanji :: String -> [Kanji]
allToKanji = map toKanji . filter isKanji
