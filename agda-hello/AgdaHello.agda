module AgdaHello where

open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤)
open import Agda.Builtin.String using (String)

postulate putStrLn : String → IO ⊤
{-# FOREIGN GHC import qualified Data.Text as T #-}
{-# FOREIGN GHC import qualified System.IO as SIO #-}
{-# COMPILE GHC putStrLn = (SIO.hPutStrLn SIO.stderr) . T.unpack #-}

main : IO ⊤
main = putStrLn "Hello world form Agda! \n"
