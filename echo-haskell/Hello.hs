module Hello where
import System.IO

foreign import ccall "hello" hello :: Int -> IO ()

main :: IO ()
main = do
  let a = "Hello form haskell" in hPutStrLn stderr a
  hello 42
