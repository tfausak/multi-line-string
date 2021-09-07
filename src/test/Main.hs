import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified GHC.Stack as Stack
import qualified MultiLineString
import qualified System.Exit as Exit
import qualified System.IO as IO

main :: Stack.HasCallStack => IO ()
main = Exception.handle handler $ do
  let f = MultiLineString.outside

  -- handles the empty string
  f "" =? ""

  -- passes through unquoted text
  f "hello world" =? "hello world"

  -- doesn't require balanced quotes
  f " \"\"\" " =? " \" "

  -- handles balanced quotes
  f " \"\"\" \"\"\" " =? " \" \" "

  -- passes through quoted text
  f " \"\"\" hello world \"\"\" " =? " \" hello world \" "

  -- escapes a double quote
  f " \"\"\" \" \"\"\" " =? " \" \\\" \" "

  -- escapes an LF
  f " \"\"\" \n \"\"\" " =? " \" \\n\\\n\\ \" "

  -- escapes a CR
  f " \"\"\" \r \"\"\" " =? " \" \\n\\\n\\ \" "

  -- escapes a CRLF
  f " \"\"\" \r\n \"\"\" " =? " \" \\n\\\n\\ \" "

  -- passes through other control characters
  f " \"\"\" \t \"\"\" " =? " \" \t \" "

  -- passes through an escaped double quote
  f " \"\"\" \\\" \"\"\" " =? " \" \\\" \" "

  -- keeps text before, inside, and after
  f " a \"\"\" b \"\"\" c " =? " a \" b \" c "

  -- doesn't escape things outside quotes
  f " \" " =? " \" "

handler :: Exception.SomeException -> IO ()
handler e = do
  IO.hPutStrLn IO.stderr $ Exception.displayException e
  Exit.exitFailure

(=?) :: (Stack.HasCallStack, Eq a, Show a) => a -> a -> IO ()
(=?) = assertEqual

assertEqual :: (Stack.HasCallStack, Eq a, Show a) => a -> a -> IO ()
assertEqual actual expected =
  Monad.unless (actual == expected)
  . Exception.throwIO
  . withCallStack
  . Exception.AssertionFailed
  $ unwords ["expected", show expected, "but got", show actual]

data WithCallStack a
  = WithCallStack Stack.CallStack a
  deriving Show

instance Eq a => Eq (WithCallStack a) where
  WithCallStack x1 x2 == WithCallStack y1 y2 =
    Stack.getCallStack x1 == Stack.getCallStack y1
    && x2 == y2

instance Exception.Exception a => Exception.Exception (WithCallStack a) where
  displayException (WithCallStack s e) =
    Exception.displayException e
    <> (if null $ Stack.getCallStack s then "" else "\n")
    <> Stack.prettyCallStack s

withCallStack :: Stack.HasCallStack => a -> WithCallStack a
withCallStack = WithCallStack Stack.callStack
