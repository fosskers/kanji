import System.Environment (getArgs)
import System.Console.GetOpt
import KanjiQ

data Flag = JapOutput

options :: [OptDescr Flag]
options = [ Option ['j'] ["japanese"] (NoArg JapOutput) jDesc ]
    where jDesc = "Output is given in Japanese instead of English."

main = do
  args <- getArgs
  opts <- processOpts args
  let (pass,fail,ks) =
          case opts of
            ([JapOutput],(ks:_)) -> (japPass,japFail,ks)
            ([],(ks:_))          -> (engPass,engFail,ks)
  results <- mapM (nanQ pass fail . toKanji) ks
  mapM_ putStrLn results
      where japPass k n = "「" ++ (show k) ++ "」は" ++ (show n) ++ "級の漢字"
            japFail k   = "「" ++ (show k) ++ "」はどの級の漢字でもない"
            engPass k n = (show k) ++ " is a " ++ (show n) ++ "th level Kanji"
            engFail k   = (show k) ++ " is not in any level."

processOpts :: [String] -> IO ([Flag],[String])
processOpts args =
    case getOpt Permute options args of
      (_,[],_)          -> error $ "No Kanji given." ++ usageMsg
      (opts,nonopts,[]) -> return (opts,nonopts)
      (_,_,errors)      -> error $ "Flag related error." ++ usageMsg
    where usageMsg = "\nUsage : nanq [OPTION] \"<kanji>\""

nanQ :: (Kanji -> Double -> String) -> (Kanji -> String) -> Kanji -> IO String
nanQ pass fail k = do
  qs <- allQs
  case whatQ k qs of
    Just qNum -> return $ pass k qNum
    Nothing   -> return $ fail k
