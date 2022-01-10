module CLI where

import System.Environment
import System.IO

import Language (Interpreter)

buildMain :: Show v => Interpreter v -> IO ()
buildMain interpreter = do
  args <- getArgs

  case parseArgs args of
    Nothing     -> putStrLn "Invalid arguments."
    (Just file) -> do
      contents <- readFile file

      case interpreter contents of
        (Left error)    -> putStrLn $ "Parsing Error: " ++ show error
        (Right Nothing) -> putStrLn "Code produces invalid output."
        (Right (Just result)) -> print result

parseArgs :: [String] -> Maybe String
parseArgs [file] = Just file
parseArgs _      = Nothing
