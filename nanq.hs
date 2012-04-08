-- TODO: Make a qDistribution function.

import System.IO (hGetContents, stdin)
import System.Environment (getArgs)
import Data.List (delete, nub)
import System.Console.GetOpt
import KanjiQ

data Language = Jap | Eng deriving (Eq)

data Flag = FileInput | PipeInput | Help | Average |
            Unknowns  | JapOutput deriving (Eq)

options :: [OptDescr Flag]
options = [ Option ['f'] ["file"]     (NoArg FileInput) fDesc
          , Option ['p'] ["pipe"]     (NoArg PipeInput) pDesc
          , Option ['h'] ["help"]     (NoArg Help)      hDesc
          , Option ['a'] ["average"]  (NoArg Average)   aDesc
          , Option ['u'] ["unknowns"] (NoArg Unknowns)  uDesc
          , Option ['j'] ["japanese"] (NoArg JapOutput) jDesc 
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
          jDesc = "Output is given in Japanese instead of English." ++
                  "\n出力の際は日本語"
          
usageMsg :: String
usageMsg = "Usage : nanq [OPTION] (kanji / file)"

main = do
  args <- getArgs
  opts <- processOpts args
  cleanedOpts <- cleanOpts opts
  case cleanedOpts of
    ([Help],_,_)            -> putStrLn $ usageInfo usageMsg options
    ([Average],_,    input) -> findAverageQ input
    ([],        lang,input) -> findQs lang input
    ([Unknowns],lang,input) -> findUnknowns lang input
    (flagsInConflict,_,_)   -> argError "Conflicting flags given."

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

findAverageQ :: String -> IO ()
findAverageQ ks = do
  qs <- allQs
  print . averageQ qs . allToKanji $ ks

findQs :: Language -> String -> IO ()
findQs lang ks = do
  results <- mapM (nanQ (getPass lang) (getFail lang)) $ allToKanji ks
  mapM_ putStrLn results
    where
      getPass Eng = \k n -> (show k) ++ " is a Level " ++ (show n) ++ " Kanji."
      getPass Jap = \k n -> "「" ++ (show k) ++ "」は" ++ (show n) ++ "級の漢字"
      getFail Eng = \k -> (show k) ++ " is not in any level."
      getFail Jap = \k -> "「" ++ (show k) ++ "」はどの級の漢字でもない"

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
    _  -> putStrLn (pass lang) >>
          mapM_ print unknowns
    where fail Eng = "No Kanji of unknown Level found."
          fail Jap = "級を明確にできない漢字は見つからなかった"
          pass Eng = "The following Kanji of unknown Level were found:"
          pass Jap = "級を明確にできなかった漢字は："

allToKanji :: String -> [Kanji]
allToKanji = map toKanji . filter isKanji
