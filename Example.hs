module Main where

import Codec.Compression.Zlib (compress, decompress)
import Prelude

main = putStrLn "Hello from rules_haskell!"

slowId = decompress . compress

-- $> 2 + 2
