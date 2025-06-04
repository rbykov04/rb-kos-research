module KosTransport where
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

foreign import ccall "syscallRecv" c_syscallRecv :: Word32 -> Ptr () -> Ptr () -> IO (Int)
foreign import ccall "syscallReply" c_syscallReply :: Word32 -> Ptr () -> Ptr () -> IO (Int)

foreign import ccall "serverLocatorConnect" c_serverLocatorConnect ::  CString -> IO Word32
foreign import ccall "serverLocatorRegister" c_serverLocatorRegister ::  CString -> IO Word32
foreign import ccall "serviceLocatorGetRiid" c_serviceLocatorGetRiid ::  Word32 -> CString -> IO (CUShort)
foreign import ccall "nkArenaInit" c_nkArenaInit ::  Ptr () -> Ptr () -> Int -> IO ()
foreign import ccall "nkArenaStoreString" c_nkArenaStoreString ::  Ptr () -> Ptr () -> Int -> CString -> Int -> IO ()
foreign import ccall "nkArenaGetString" c_nkArenaGetString  :: Ptr () -> Ptr () -> Int -> IO (CString)

foreign import ccall "nkFillEnvelope" c_nkFillVenvelope :: Ptr () -> Int -> CUShort -> CUShort -> CUInt -> IO ()
foreign import ccall "nkEnvelopeMid" c_nkEnvelopeMid :: Ptr () -> IO (CUShort)
foreign import ccall "nkEnvelopeRiid" c_nkEnvelopeRiid :: Ptr () -> IO (CUShort)
foreign import ccall "nkSetEnvelopeMid" c_nkSetEnvelopeMid :: Ptr () -> CUShort-> IO ()
foreign import ccall "nkSetEnvelopeRiid" c_nkSetEnvelopeRiid :: Ptr () -> CUShort -> IO ()


data Handle = Handle Word32
data Riid = Riid CUShort
data Mid = Mid CUShort

serverLocatorConnect :: String -> IO (Handle)
serverLocatorConnect connection = do
  h <- withCAString connection c_serverLocatorConnect
  return $ Handle h

serverLocatorRegister :: String -> IO (Handle)
serverLocatorRegister connection = do
  h <- withCAString connection c_serverLocatorRegister
  return $ Handle h

serviceLocatorGetRiid :: Handle -> String -> IO (Riid)
serviceLocatorGetRiid (Handle h) endpoint = do
  r <- withCAString endpoint (c_serviceLocatorGetRiid h)
  return $ Riid r


arenaStructSize :: Int
arenaStructSize = 3 * 8 --HACK?

getEnvelopeMid :: KosStorage -> IO (Mid)
getEnvelopeMid (KosStorage envelope _) = do
   mid <- c_nkEnvelopeMid envelope
   return (Mid mid)

getEnvelopeRiid :: KosStorage -> IO (Riid)
getEnvelopeRiid (KosStorage envelope _) = do
   mid <- c_nkEnvelopeRiid envelope
   return (Riid mid)

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

data KosStorage = KosStorage (Ptr ()) (Ptr ())

storeString :: KosStorage -> Int -> String -> IO ()
storeString (KosStorage req reqArena) offset text = do
    withCAString text   $ \ s ->
      c_nkArenaStoreString reqArena req offset s (length text)


getString  :: KosStorage -> Int -> IO (String)
getString (KosStorage req reqArena) offset = do
      c_str <- c_nkArenaGetString req reqArena offset
      text <- peekCAString c_str
      return text



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


recv :: Handle -> KosStorage -> IO Int
recv (Handle h) (KosStorage req reqArena) =
    c_syscallRecv h req reqArena

reply :: Handle -> KosStorage -> IO Int
reply (Handle h) (KosStorage res resArena) =
    c_syscallReply h res resArena
