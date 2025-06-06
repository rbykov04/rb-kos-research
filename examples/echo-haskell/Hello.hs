module Hello where
import KosTransport
import Echo

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
