module MultiLineString where

import qualified System.Environment as Environment

defaultMain :: IO ()
defaultMain = do
  name <- Environment.getProgName
  arguments <- Environment.getArgs
  mainWith name arguments

mainWith :: String -> [String] -> IO ()
mainWith _ arguments = case arguments of
  [] -> interact outside
  _ : input : output : _ -> do
    contents <- readFile input
    writeFile output $ outside contents
  _ -> fail $ "unexpected arguments: " <> show arguments

outside :: String -> String
outside xs = case xs of
  '"' : '"' : '"' : ys -> '"' : inside ys
  x : ys -> x : outside ys
  [] -> xs

inside :: String -> String
inside xs = case xs of
  '"' : '"' : '"' : ys -> '"' : outside ys
  '\\' : '"' : ys -> "\\\"" <> inside ys
  '\r' : '\n' : ys -> "\\n\\\n\\" <> inside ys
  x : ys ->
    let
      y = case x of
        '\n' -> "\\n\\\n\\"
        '\r' -> "\\n\\\n\\"
        '\"' -> "\\\""
        _ -> [x]
    in y <> inside ys
  [] -> xs
