import System.Environment (getArgs)
import KanjiQ

main = do
  (ks:_)  <- getArgs
  results <- mapM (nanQ . toKanji) ks
  mapM_ putStrLn results

nanQ :: Kanji -> IO String
nanQ k = do
  qs <- allQs
  case whatQ k qs of
    Left eMsg  -> return eMsg  -- The Kanji wasn't found.
    Right qNum -> return $ (show k) ++ " is a " ++ (show qNum) ++ "級 Kanji"
