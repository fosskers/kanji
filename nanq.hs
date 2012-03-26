import System.Environment (getArgs)
import KanjiQ

main = do
  (ks:_) <- getArgs
  results <- mapM (nanQ . toKanji . toString) ks
  mapM_ putStrLn results

toString :: Char -> String
toString c = [c]

nanQ :: Kanji -> IO String
nanQ k = do
  qs <- allQs
  case whatQ k qs of
    Left eMsg  -> return eMsg  -- The Kanji wasn't found.
    Right qNum -> return $ (showK k) ++ " is a " ++ (show qNum) ++ "級 Kanji"
