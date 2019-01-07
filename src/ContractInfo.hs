{-# OPTIONS_GHC -funbox-strict-fields #-}

module ContractInfo
  ( ContractInfo(..)
  , Code
  , Address
  , fromText
  , toReverseBinary
  , fromReverseBinary
  ) where

import           Control.Monad (forM_)
import           Data.Char (ord)
import           Data.Word (Word8)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector.Unboxed as U
import           Data.Binary.Put
import           Data.Binary.Get
import           System.IO (withBinaryFile, IOMode(..), Handle)


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


toReverseBinary :: FilePath -> Map.Map Code [Address] -> IO ()
toReverseBinary name cm = withBinaryFile name WriteMode $ \h -> do
  forM_ (Map.toList cm) $ \(code, addrs)  -> do
    L.hPut h $ runPut $ do
      putInt32le $ fromIntegral $ length addrs
      forM_ addrs (sequence . map putWord8 . U.toList)
      putInt32le $ fromIntegral $ U.length code
      mapM_ putWord8 $ U.toList code


fromReverseBinary :: FilePath -> IO [(Code, [Address])]
fromReverseBinary = fmap loop . L.readFile
  where
    getEntry = do
      addrCount <- fromIntegral <$> getInt32le
      addrs <- sequence $ replicate addrCount
        $ U.fromListN (fromIntegral const_ADDRESS_LENGTH)
          <$> sequence (replicate const_ADDRESS_LENGTH getWord8)
      codeLen <- fromIntegral <$> getInt32le
      code <- U.fromListN codeLen <$> sequence (replicate codeLen getWord8)
      return (code, addrs)

    loop str
      | L.null str = []
      | otherwise = case runGetOrFail getEntry str of
        Left err -> error $ show err
        Right (rest, _, res) -> res : loop rest


-- TODO: read Word16 from LBS, search it in precompiled IntMap
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
      = let !x = fromIntegral $ (hexDigit a) * 16 + hexDigit b
        in x : loop xs
    loop _ = []

hexDigit :: Char -> Int
hexDigit c
  | '0' <= c && c <= '9' = ord c - ord '0'
  | 'a' <= c && c <= 'f' = ord c - ord 'a' + 10
  | 'A' <= c && c <= 'F' = ord c - ord 'A' + 10
  | otherwise            = error "Invalid hex digit"
