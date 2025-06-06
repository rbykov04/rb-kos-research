module Hello where
import System.IO

test :: Int
test = 10

main :: IO ()
main = do
  let a = "Hello form haskell" ++ show test in hPutStrLn stderr a
