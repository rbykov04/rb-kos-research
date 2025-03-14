module Hello where
import Foreign.C.String
import Data.Word

foreign import ccall "hello" c_hello ::  Word32 -> CString -> Int -> IO (Int)
foreign import ccall "serverLocatorConnect" c_serverLocatorConnect ::  CString -> IO (Word32)

data Handle = Handle Word32

serverLocatorConnect :: String -> IO (Handle)
serverLocatorConnect connection = do
  h <- withCAString connection c_serverLocatorConnect
  return $ Handle h

say :: Handle -> String -> IO (Int)
say (Handle h) text = do
  withCAString text   $ \ s ->
      c_hello h s (length text)



main :: IO (Int)
main = do
  h <- serverLocatorConnect "server_connection"
  say h "Hello From Haskell "
  say h "1 Hello From Haskell "
  say h "2 Hello From Haskell "
  say h "3 Hello From Haskell "
  say h "4 Hello From Haskell "
  say h "5 Hello From Haskell "
  say h "6 Hello From Haskell "
