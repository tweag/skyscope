module Main where

import Codec.Compression.Zlib (compress, decompress)
import Prelude
import Data.Aeson

main = putStrLn "Hello from rules_haskell!"

slowId = decompress . compress

-- $> 2 + 2 + 1
