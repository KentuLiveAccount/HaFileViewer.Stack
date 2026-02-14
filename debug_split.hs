{-# LANGUAGE OverloadedStrings #-}
-- Debug script to trace line extraction
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Word (Word8)

lfByte :: Word8
lfByte = 10

main :: IO ()
main = do
  putStrLn "Testing BS.split on various inputs:"
  
  let test1 = "a\nb\n"
  putStrLn $ "\nInput: " ++ show test1
  putStrLn $ "Split result: " ++ show (BS.split lfByte test1)
  
  let test2 = "a\nb\nc"
  putStrLn $ "\nInput: " ++ show test2
  putStrLn $ "Split result: " ++ show (BS.split lfByte test2)
  
  let test3 = "\n\n\n"
  putStrLn $ "\nInput: " ++ show test3
  putStrLn $ "Split result: " ++ show (BS.split lfByte test3)
  
  let test4 = "a\n\nb\n"
  putStrLn $ "\nInput: " ++ show test4
  putStrLn $ "Split result: " ++ show (BS.split lfByte test4)
  
  putStrLn "\n\nTesting tail . init:"
  let pieces1 = ["a", "b", ""]
  putStrLn $ "pieces: " ++ show pieces1
  putStrLn $ "tail . init: " ++ show ((tail . init) pieces1)
