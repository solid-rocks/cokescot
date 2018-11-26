{-# OPTIONS_GHC -funbox-strict-fields #-}

module BinaryFormat where

import           Control.Applicative ((<|>))
import           Control.Monad
import           Data.Maybe (fromMaybe)

import           Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Internal.Read (hexDigitToInt)

import           Data.Binary.Put (putLazyByteString, putInt32le, runPut)

import           System.IO (stderr, IOMode(WriteMode), withBinaryFile)


-- const_ADDRESS_LENGTH = 20 :: Int
type Address = B.ByteString
type Code = B.ByteString


data ContractInfo = ContractInfo
  { addr :: !Address
  , code :: !Code
  }


-- FIXME: handle file open/write errors
-- FIXME: handle format errors
-- FIXME: check addr length
jsonToBinary :: FilePath -> IO ()
jsonToBinary out = withBinaryFile out WriteMode $ \h -> do
  let parser jsn = (,) <$> jsn .: "address" <*> jsn .: "bytecode"
  let parseRow :: L8.ByteString -> Maybe (Text, Text)
      parseRow r = Aeson.decode r >>= Aeson.parseMaybe parser

  rows <- L8.lines <$> L8.getContents
  forM_ rows $ \r -> case parseRow r of
    Nothing -> L8.hPutStr stderr r
    Just (addrHex, codeHex) -> do
      let addr' = hexToLBS addrHex
      let code' = hexToLBS codeHex
      -- I tried to reduce number of hPut's by collecting
      -- bunch of strings and writing them all at once
      -- but this does not influence performance at all.
      L.hPut h $ runPut $ do
        putLazyByteString addr'
        putInt32le $ fromIntegral $ L.length code'
        putLazyByteString code'


hexToLBS :: Text -> L.ByteString
hexToLBS hex
  = B.toLazyByteString
  $ mconcat $ map readByte
  $ case T.length hex' `mod` 2 of
    1 -> T.snoc "0" (T.head hex') : T.chunksOf 2 (T.tail hex')
    _ -> T.chunksOf 2 hex'
  where
    hex' = fromMaybe hex
      $   T.stripPrefix "0x" hex
      <|> T.stripPrefix "0X" hex
    readByte byte = B.word8
      $ fromIntegral
      $ hexDigitToInt (T.head byte) * 16 + hexDigitToInt (T.last byte)
