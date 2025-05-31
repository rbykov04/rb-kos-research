module Server where
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.Word
import KosTransport
import Echo
import System.IO (hPutStrLn , stderr)


    --(Riid (CUShort riid)) <- getEnvelopeRiid req_
    --(Mid (CUShort mid))  <- getEnvelopeMid req_
    -- FIXME: Problem in access to mid
    --hPutStrLn stderr $ "mid ==" ++  show mid ++ "\n"


serverMain :: Handle -> KosStorage -> KosStorage -> IO ()
serverMain handle req res = do
    recv handle req
    text <- getString req valueOffset
    hPutStrLn stderr $ text
    reply handle res
    return ()

loop :: Handle -> IO ()
loop h = do
  withKosRpcMessage Echo (Riid (CUShort 0)) (serverMain h)
  loop h

main :: IO ()
main = do
  h <- serverLocatorRegister "server_connection"
  loop h
