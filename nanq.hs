import System.Environment (getArgs)
import System.Console.GetOpt
import KanjiQ

data Flag = Help | Average | JapOutput

options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help) hDesc
          , Option ['a'] ["average"] (NoArg Average) aDesc
          , Option ['j'] ["japanese"] (NoArg JapOutput) jDesc 
          ]
    where hDesc = "Shows this help message."
          aDesc = "Find the average level of some given line of Japanese." ++
                  "\n日本語の文の平均的な級を求める"
          jDesc = "Output is given in Japanese instead of English." ++
                  "\n出力の際は日本語"
          
usageMsg :: String
usageMsg = "Usage : nanq [OPTION] <kanji>"

main = do
  args <- getArgs
  opts <- processOpts args
  case opts of
    ([Help],_)           -> putStrLn $ usageInfo usageMsg options
    (_,[])               -> argError "No Kanji given."
    ([Average],  (ks:_)) -> findAverageQ ks
    ([JapOutput],(ks:_)) -> findQ japPass japFail ks
    ([],         (ks:_)) -> findQ engPass engFail ks  -- English by default.
    (conflictingFlags,_) -> argError "Conflicting flags given."
    where japPass k n = "「" ++ (show k) ++ "」は" ++ (show n) ++ "級の漢字"
          japFail k   = "「" ++ (show k) ++ "」はどの級の漢字でもない"
          engPass k n = (show k) ++ " is a Level " ++ (show n) ++ " Kanji"
          engFail k   = (show k) ++ " is not in any level."

processOpts :: [String] -> IO ([Flag],[String])
processOpts args =
    case getOpt Permute options args of
      (opts,nonopts,[]) -> return (opts,nonopts)
      (_,_,errors)      -> argError "Bad flag used."

argError :: String -> a
argError msg = error $ usageInfo (msg ++ "\n" ++ usageMsg) options

findAverageQ :: String -> IO ()
findAverageQ ks = do
  qs <- allQs
  print . averageQ qs . map toKanji . filter isKanji $ ks

findQ :: (Kanji -> Double -> String) -> (Kanji -> String) -> String -> IO ()
findQ pass fail ks = do
  results <- mapM (nanQ pass fail . toKanji) . filter isKanji $ ks
  mapM_ putStrLn results

nanQ :: (Kanji -> Double -> String) -> (Kanji -> String) -> Kanji -> IO String
nanQ pass fail k = do
  qs <- allQs
  case whatQ qs k of
    Just qNum -> return $ pass k qNum
    Nothing   -> return $ fail k
