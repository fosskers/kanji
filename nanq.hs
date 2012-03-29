-- TODO: Make a help option.

import System.Environment (getArgs)
import System.Console.GetOpt
import KanjiQ

data Flag = Average | JapOutput

options :: [OptDescr Flag]
options = [ Option ['a'] ["average"] (NoArg Average) aDesc
          , Option ['j'] ["japanese"] (NoArg JapOutput) jDesc 
          ]
    where jDesc = "Output is given in Japanese instead of English." ++
                  "出力の際は日本語"
          aDesc = "Find the average level of some given line of Japanese" ++
                  "日本語の文の平均的な級を求める"

main = do
  args <- getArgs
  opts <- processOpts args
  case opts of
    ([Average],(ks:_))   -> findAverageQ ks
    ([JapOutput],(ks:_)) -> findQ japPass japFail ks
    ([],(ks:_))          -> findQ engPass engFail ks
    where japPass k n = "「" ++ (show k) ++ "」は" ++ (show n) ++ "級の漢字"
          japFail k   = "「" ++ (show k) ++ "」はどの級の漢字でもない"
          engPass k n = (show k) ++ " is a Level " ++ (show n) ++ " Kanji"
          engFail k   = (show k) ++ " is not in any level."

processOpts :: [String] -> IO ([Flag],[String])
processOpts args =
    case getOpt Permute options args of
      (_,[],_)          -> error $ "No Kanji given." ++ usageMsg
      (opts,nonopts,[]) -> return (opts,nonopts)
      (_,_,errors)      -> error $ "Flag related error." ++ usageMsg
    where usageMsg = "\nUsage : nanq [OPTION] \"<kanji>\""

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
