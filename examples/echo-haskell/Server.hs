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

echoRiid :: Riid
echoRiid = Riid (CUShort 0)

serverMain :: Handle -> KosStorage -> KosStorage -> IO ()
serverMain handle req res = do
    recv handle req
    (Mid (CUShort mid))  <- getEnvelopeMid req
    if mid == 0
    then do
      text <- getString req valueOffset
      hPutStrLn stderr $ text
      reply handle res
      return ()
    else do
      hPutStrLn stderr $ "Mid is not correct. mid == "  ++  show mid ++ "\n"
      error "panic"
      return ()

loop :: Handle -> IO ()
loop h = do
  withKosRpcMessage Echo echoRiid (serverMain h)
  loop h

main :: IO ()
main = do
  h <- serverLocatorRegister "server_connection"
  loop h
