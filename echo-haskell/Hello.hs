module Hello where
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.Word
import System.IO

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
data Mid = Mid CUShort

serverLocatorConnect :: String -> IO (Handle)
serverLocatorConnect connection = do
  h <- withCAString connection c_serverLocatorConnect
  return $ Handle h

serviceLocatorGetRiid :: Handle -> String -> IO (Riid)
serviceLocatorGetRiid (Handle h) endpoint = do
  r <- withCAString endpoint (c_serviceLocatorGetRiid h)
  return $ Riid r


arenaStructSize :: Int
arenaStructSize = 3 * 8 --HACK?

valueOffset :: Int
valueOffset = 24

withEnvelope :: forall a . Int -> Riid -> Mid -> CUInt -> (Ptr () -> IO a) -> IO a
withEnvelope size (Riid riid) (Mid mid) ncaps io =
  allocaBytes size $ \ req -> do
    c_nkFillVenvelope req size riid mid ncaps
    io req

withArena :: forall a . Int -> ( Ptr () -> IO a ) -> IO a
withArena size io =
    allocaBytes arenaStructSize $ \ arenaReq ->
          allocaBytes size $ \ buf -> do
            c_nkArenaInit arenaReq buf size
            io arenaReq

class KosMessageStaticDesc a where
  getMid            :: a -> Mid

  requestSize       :: a -> Int
  requestNCaps      :: a -> CUInt
  requestArenaSize  :: a -> Int

  responseSize      :: a -> Int
  responseNCaps     :: a -> CUInt
  responseArenaSize :: a -> Int

data Echo = Echo

instance KosMessageStaticDesc Echo where
  getMid            _ = Mid (CUShort 0)

  requestSize       _ = 32
  requestNCaps      _ = CUInt 0
  requestArenaSize  _ = 257

  responseSize      _ = 24
  responseNCaps     _ = CUInt 0
  responseArenaSize _ = 0

data KosStorage = KosStorage (Ptr ()) (Ptr ())

storeString :: KosStorage -> Int -> String -> IO ()
storeString (KosStorage req reqArena) offset text = do
    withCAString text   $ \ s ->
      c_nkArenaStoreString reqArena req offset s (length text)

withKosRpcMessage :: forall a b. (KosMessageStaticDesc a) =>
  a -> Riid -> (KosStorage  -> KosStorage  -> IO b) -> IO b
withKosRpcMessage obj riid io =
  withEnvelope (requestSize obj)  riid (getMid obj) (requestNCaps obj) $ \ req ->
    withEnvelope (responseSize obj) riid (getMid obj) (responseNCaps obj) $ \ res ->
      withArena (requestArenaSize obj) $ \ arenaReq ->
        withArena (responseArenaSize obj) $ \ arenaRes ->
   io (KosStorage req arenaReq) (KosStorage res arenaRes)

call :: Handle -> KosStorage -> KosStorage -> IO Int
call (Handle h) (KosStorage req reqArena) (KosStorage res resArena) =
    c_syscallCall h req reqArena res resArena

say :: Handle -> Riid -> String -> IO Int
say handle riid text =
  withKosRpcMessage Echo riid $ \ req res -> do
    storeString req valueOffset text
    call handle req res


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
