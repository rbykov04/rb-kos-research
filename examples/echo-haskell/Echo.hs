module Echo where
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.Word

import KosTransport

data Echo = Echo

instance KosMessageStaticDesc Echo where
  getMid            _ = Mid (CUShort 0)

  requestSize       _ = 32
  requestNCaps      _ = CUInt 0
  requestArenaSize  _ = 257

  responseSize      _ = 24
  responseNCaps     _ = CUInt 0
  responseArenaSize _ = 0

valueOffset :: Int
valueOffset = 24
