{-# OPTIONS_GHC -funbox-strict-fields #-}

module ContractInfo
  ( ContractInfo(..)
  , fromText
  ) where

import           Data.Char (ord)
import           Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Vector.Unboxed as U

import           System.IO (Handle)


const_ADDRESS_LENGTH :: Int
const_ADDRESS_LENGTH = 20

type Address = U.Vector Word8
type Code = U.Vector Word8


data ContractInfo = ContractInfo
  { addr :: !Address
  , code :: !Code
  }


instance Show ContractInfo where
  show ContractInfo{..}
    = L8.unpack $ B.toLazyByteString
    $ B.byteStringHex (B.pack $ U.toList addr)
      <> B.char8 ' '
      <> B.byteStringHex (B.pack $ U.toList code)


fromText :: Handle -> IO [ContractInfo]
fromText h = do
  let parse = \case
        -- TODO: check addr length
        -- TODO: check if valid hex
        [addr, code] -> ContractInfo (hexToVec addr) (hexToVec code)
        [addr] -> ContractInfo (hexToVec addr) U.empty
        line -> error $ show line
  map (parse . L8.words) . L8.lines <$> L8.hGetContents h


hexToVec :: L8.ByteString -> U.Vector Word8
hexToVec hex
  = U.fromListN (fromIntegral $ len' `div` 2) $ loop hex'
  where
    evenify res@(len, xs) = if even len then res else (len + 1, '0' : xs)
    (len', hex') = evenify $ case (L8.length hex, L8.unpack hex) of
      (len, '0':'x':xs) -> (len - 2, xs)
      (len, '0':'X':xs) -> (len - 2, xs)
      res -> res
    loop (a:b:xs)
      = let x = (hexDigit a) * 16 + hexDigit b
        in fromIntegral x : loop xs
    loop _ = []


hexDigit :: Char -> Int
hexDigit c
  | '0' <= c && c <= '9' = ord c - ord '0'
  | 'a' <= c && c <= 'f' = ord c - ord 'a' + 10
  | 'A' <= c && c <= 'F' = ord c - ord 'A' + 10
  | otherwise            = error "Invalid hex digit"
