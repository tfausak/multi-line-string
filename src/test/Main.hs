import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified MultiLineString

main :: IO ()
main = do
  MultiLineString.outside "" =? ""
  MultiLineString.outside " \"\"\" \"\"\" " =? " \" \" "
  MultiLineString.outside " \"\"\" \n hello \n world \n \"\"\" " =? " \" \\n hello \\n world \\n \" "
  MultiLineString.outside " \"\"\" " =? " \" "
  MultiLineString.outside " \"\"\" \t \\t \"\"\" " =? " \" \t \\t \" "
  MultiLineString.outside " \"\"\" \" \"\"\" " =? " \" \\\" \" "
  MultiLineString.outside " \"\"\" a \"\"\" \"\"\" b \"\"\" " =? " \" a \" \" b \" "

(=?) :: (Eq a, Show a) => a -> a -> IO ()
(=?) = assertEqual

assertEqual :: (Eq a, Show a) => a -> a -> IO ()
assertEqual actual expected =
  Monad.unless (actual == expected)
  . Exception.throwIO
  . Exception.AssertionFailed
  $ unwords ["expected", show expected, "but got", show actual]
