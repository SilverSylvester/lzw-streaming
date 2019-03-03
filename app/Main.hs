module Main where

import Prelude
import qualified LZW

main :: IO ()
main = do
  putStrLn "Compressing ..."
  LZW.compressFile "resources/big.txt" "resources/big-compressed"
  putStrLn "Decompressing ..."
  LZW.decompressFile "resources/big-compressed" "resources/big-decompressed.txt"
  putStrLn "Done!"
