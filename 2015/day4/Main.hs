module Main where
import System.Environment (getArgs)
import Crypto.Hash.MD5
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (intToDigit)
import Data.Word (Word8)
import Data.Maybe (fromJust)
import Data.Bits
import Data.List

byteHex :: Word8 -> String
byteHex b = 
  map intToDigit [ fromIntegral b `shiftR` 4, fromIntegral b .&. 0xf]

showHex :: [Word8] -> String
showHex bs = concatMap byteHex bs

hashString :: String -> String
hashString s = showHex $ B.unpack $ hash $ BC.pack s

solve :: String -> Int -> Int
solve input digits = fromJust $ find valid [1..]
  where valid n = (replicate digits '0') == (take digits $ hashString $ input ++ show n)

process1 :: String -> Int
process1 input = solve input 5

process2 :: String -> Int
process2 input = solve input 6

main = do
  [input] <- getArgs
  print $ process1 input
  print $ process2 input