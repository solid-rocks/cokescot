{-# OPTIONS_GHC -funbox-strict-fields #-}

module ContractInfo
  ( ContractInfo(..)
  , fromText
  ) where

import           Data.Char (ord)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

import           System.IO (Handle)


const_ADDRESS_LENGTH :: Int
const_ADDRESS_LENGTH = 20

type Address = B.ByteString
type Code = B.ByteString


data ContractInfo = ContractInfo
  { addr :: !Address
  , code :: !Code
  }


instance Show ContractInfo where
  show ContractInfo{..}
    = L8.unpack $ B.toLazyByteString
    $ B.byteStringHex addr <> B.char8 ' ' <> B.byteStringHex code


fromText :: Handle -> IO [ContractInfo]
fromText h = do
  let parse = \case
        -- TODO: check addr length
        [addr, code] -> ContractInfo (hexToBS addr) (hexToBS code)
        [addr] -> ContractInfo (hexToBS addr) B.empty
        line -> error $ show line
  map (parse . L8.words) . L8.lines <$> L8.hGetContents h


-- FIXME: may fail
hexToBS :: L.ByteString -> B.ByteString
hexToBS hex
  = L.toStrict $ B.toLazyByteString $ mconcat $ map (B.word8 . fromIntegral)
  $ loop hex'
  where
    evenify xs = if even $ L.length hex then xs else '0':xs
    hex' = evenify $ case L8.unpack hex of
      '0':'x':xs -> xs
      '0':'X':xs -> xs
      xs -> xs
    loop (a:b:xs) = (hexDigit a) * 16 + hexDigit b : loop xs
    loop _ = []

hexDigit :: Char -> Int
hexDigit c
  | '0' <= c && c <= '9' = ord c - ord '0'
  | 'a' <= c && c <= 'f' = ord c - ord 'a' + 10
  | 'A' <= c && c <= 'F' = ord c - ord 'A' + 10
  | otherwise            = error "Invalid hex digit"
