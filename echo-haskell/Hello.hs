module Hello where
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.Word

foreign import ccall "hello" c_hello ::  Word32 -> Word16 -> CString -> Int -> Ptr Char -> IO (Int)
foreign import ccall "serverLocatorConnect" c_serverLocatorConnect ::  CString -> IO (Word32)
foreign import ccall "serviceLocatorGetRiid" c_serviceLocatorGetRiid ::  Word32 -> CString -> IO (Word16)

data Handle = Handle Word32
data Riid = Riid Word16 -- Here must be uint16_t

serverLocatorConnect :: String -> IO (Handle)
serverLocatorConnect connection = do
  h <- withCAString connection c_serverLocatorConnect
  return $ Handle h

serviceLocatorGetRiid :: Handle -> String -> IO (Riid)
serviceLocatorGetRiid (Handle h) endpoint = do
  r <- withCAString endpoint (c_serviceLocatorGetRiid h)
  return $ Riid r


constEchoEchoReqArenaSize :: Int
constEchoEchoReqArenaSize  = 257

say :: Handle -> Riid -> String -> IO (Int)
say (Handle h) (Riid r) text = do
  allocaBytes constEchoEchoReqArenaSize $ \ buf ->  -- TODO: clean buf
    withCAString text   $ \ s ->
        c_hello h r s (length text) buf

data Arena = Arena
  { startArena :: Ptr ()
  , currentArena :: Ptr ()
  , endArena :: Ptr ()
  }

type ArenaPtr = Ptr Arena

main :: IO (Int)
main = do
  h <- serverLocatorConnect "server_connection"
  r <- serviceLocatorGetRiid h "Server.main"
  say h r "0 Hello From Haskell "
  say h r "1 Hello From Haskell "
  say h r "2 Hello From Haskell "
  say h r "3 Hello From Haskell "
  say h r "4 Hello From Haskell "
  say h r "5 Hello From Haskell "
  say h r "6 Hello From Haskell "
