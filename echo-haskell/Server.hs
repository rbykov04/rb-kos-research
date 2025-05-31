module Server where
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.Word
import System.IO

foreign import ccall "serverMain" c_serverMain :: IO ()

main :: IO ()
main = do
  c_serverMain
