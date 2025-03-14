module Hello where
import System.IO
import Foreign.C.String

foreign import ccall "hello" c_hello ::  CString -> CString -> Int -> IO (Int)

say :: String -> IO (Int)
say text = do
  withCAString text                $ \ s ->
    withCAString "server_connection" $ \ connection ->
      c_hello connection s (length text)

main :: IO (Int)
main = do
  say "Hello From Haskell "
  say "1 Hello From Haskell "
  say "2 Hello From Haskell "
  say "3 Hello From Haskell "
  say "4 Hello From Haskell "
  say "5 Hello From Haskell "
  say "6 Hello From Haskell "
