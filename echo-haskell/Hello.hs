module Hello where
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.Word

foreign import ccall "syscallCall" c_syscallCall ::
                    Word32 ->
                    Ptr () -> Ptr () ->
                    Ptr () -> Ptr () ->
                    IO (Int)
foreign import ccall "serverLocatorConnect" c_serverLocatorConnect ::  CString -> IO Word32
foreign import ccall "serviceLocatorGetRiid" c_serviceLocatorGetRiid ::  Word32 -> CString -> IO (Word16)
foreign import ccall "nkArenaInit" c_nkArenaInit ::  Ptr () -> Ptr () -> Int -> IO ()
foreign import ccall "nkArenaStoreString" c_nkArenaStoreString ::  Ptr () -> Ptr () -> Int -> CString -> Int -> IO ()
foreign import ccall "nkFillEnvelope" c_nkFillVenvelope :: Ptr () -> Int -> Word16 -> CUShort -> CUInt -> IO ()


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


arenaStructSize :: Int
arenaStructSize = 3 * 8 --HACK?

mid :: CUShort
mid = CUShort 0

ncaps :: CUInt
ncaps = CUInt 0

say :: Handle -> Riid -> String -> IO (Int)
say (Handle h) (Riid r) text = do
  allocaBytes 32 $ \ req -> do
    c_nkFillVenvelope req 32 r mid ncaps
    allocaBytes 24 $ \ res -> do
      c_nkFillVenvelope res 24 r mid ncaps
      allocaBytes arenaStructSize $ \ arenaReq ->
        allocaBytes constEchoEchoReqArenaSize $ \ buf ->
          withCAString text   $ \ s -> do
              c_nkArenaInit arenaReq buf constEchoEchoReqArenaSize
              c_nkArenaStoreString arenaReq req 24 s (length text)
              c_syscallCall h req arenaReq res nullPtr

main :: IO Int
main = do
  h <- serverLocatorConnect "server_connection"
  r <- serviceLocatorGetRiid h "Server.main"
  say h r "0 Hello From Haskell \0"
  say h r "1 Hello From Haskell \0"
  say h r "2 Hello From Haskell \0"
  say h r "3 Hello From Haskell \0"
  say h r "4 Hello From Haskell \0"
  say h r "5 Hello From Haskell \0"
  say h r "6 Hello From Haskell \0"
