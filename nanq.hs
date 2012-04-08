-- TODO: Make a qDistribution function.

import System.IO (hGetContents, stdin)
import System.Environment (getArgs)
import Data.List (delete, nub)
import System.Console.GetOpt
import KanjiQ

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
    ([Help],_)          -> putStrLn $ usageInfo usageMsg options
    ([Average],  input) -> findAverageQ input
    ([JapOutput],input) -> findQs japPass japFail input
    ([],         input) -> findQs engPass engFail input
    ([Unknowns], input) -> findUnknowns input
    (flagsInConflict,_) -> argError "Conflicting flags given."
    where japPass k n = "「" ++ (show k) ++ "」は" ++ (show n) ++ "級の漢字"
          japFail k   = "「" ++ (show k) ++ "」はどの級の漢字でもない"
          engPass k n = (show k) ++ " is a Level " ++ (show n) ++ " Kanji"
          engFail k   = (show k) ++ " is not in any level."

processOpts :: [String] -> IO ([Flag],[String])
processOpts args =
    case getOpt Permute options args of
      (opts,nonopts,[]) -> return (opts,nonopts)
      (_,_,errors)      -> argError "Bad flag used."

-- Determine how the input is to be received, and get it.
cleanOpts :: ([Flag],[String]) -> IO ([Flag],String)
cleanOpts (opts,nonopts)
    | Help `elem` opts      = return ([Help],"")  -- Disregards everything else.
    | PipeInput `elem` opts = do input <- hGetContents stdin
                                 return (delete PipeInput opts, input)
    | null nonopts          = argError "No Kanji / file given!"
    | FileInput `elem` opts = do input <- readFile $ head nonopts
                                 return (delete FileInput opts, input)
    | otherwise             = return (opts, head nonopts)

argError :: String -> a
argError msg = error $ usageInfo (msg ++ "\n" ++ usageMsg) options

findAverageQ :: String -> IO ()
findAverageQ ks = do
  qs <- allQs
  print . averageQ qs . allToKanji $ ks

findQs :: (Kanji -> Double -> String) -> (Kanji -> String) -> String -> IO ()
findQs pass fail ks = do
  results <- mapM (nanQ pass fail) $ allToKanji ks
  mapM_ putStrLn results

nanQ :: (Kanji -> Double -> String) -> (Kanji -> String) -> Kanji -> IO String
nanQ pass fail k = do
  qs <- allQs
  case whatQ qs k of
    Just qNum -> return $ pass k qNum
    Nothing   -> return $ fail k

findUnknowns :: String -> IO ()
findUnknowns ks = do
  qs <- allQs
  let unknowns = nub . filter (isUnknown qs) . allToKanji $ ks
  case unknowns of
    [] -> putStrLn "No Kanji of unknown Level found."
    _  -> putStrLn "The following Kanji of unknown Level were found:" >>
          mapM_ print unknowns

allToKanji :: String -> [Kanji]
allToKanji = map toKanji . filter isKanji
