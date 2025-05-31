module Server where
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.Word
import KosTransport
import Echo

foreign import ccall "serverMain" c_serverMain ::
                    Word32 ->
                    Ptr () -> Ptr () ->
                    Ptr () -> Ptr () ->
                    IO ()

serverMain :: Handle -> KosStorage -> KosStorage -> IO ()
serverMain (Handle h) (KosStorage req reqArena) (KosStorage res resArena) = do
    c_serverMain h req reqArena res resArena

main :: IO ()
main = do
  h <- serverLocatorRegister "server_connection"
  withKosRpcMessage Echo (Riid (CUShort 111)) (serverMain h)
