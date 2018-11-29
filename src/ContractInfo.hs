{-# OPTIONS_GHC -funbox-strict-fields #-}

module ContractInfo
  ( ContractInfo(..)
  , fromJSON, fromBinary, toBinary
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad
import           Control.Exception
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

import           Data.Binary
import           Data.Binary.Get (getByteString, getInt32le)
import           Data.Binary.Put (putByteString, putInt32le, runPut)

import           System.IO (IOMode(WriteMode), withBinaryFile, Handle)


const_ADDRESS_LENGTH :: Int
const_ADDRESS_LENGTH = 20

type Address = B.ByteString
type Code = B.ByteString


data ContractInfo = ContractInfo
  { addr :: !Address
  , code :: !Code
  }


instance Show ContractInfo where
  show ContractInfo{..} = show $ B.toLazyByteString
    $ B.byteStringHex addr <> B.char8 ' ' <> B.byteStringHex code


instance Binary ContractInfo where
  get = do
    addr' <- getByteString const_ADDRESS_LENGTH
    len   <- getInt32le
    code' <- getByteString $ fromIntegral len
    return $ ContractInfo addr' code'

  put ContractInfo{..} = do
    putByteString addr
    putInt32le $ fromIntegral $ B.length code
    putByteString code


data ParseError = ParseError L.ByteString String deriving Show
instance Exception ParseError


fromBinary :: FilePath -> IO [ContractInfo]
fromBinary f = loop <$> L.readFile f
  where
    loop bytes = case decodeOrFail bytes of
      Left ("", _consumed, _err) -> []
      Left (rest, _consumed, err) -> throw $ ParseError (L.take 200 rest) err
      Right (rest, _consumed, res) -> res : loop rest


fromJSON :: Handle -> IO [ContractInfo]
fromJSON h = do
  let parser jsn = do
        addr' <- hexToBS <$> jsn .: "address"
        code' <- hexToBS <$> jsn .: "bytecode"
        when (B.length addr' /= const_ADDRESS_LENGTH)
          $ fail "invalid address length"
        return $ ContractInfo addr' code'
  let parseRow r = Aeson.eitherDecode r >>= Aeson.parseEither parser

  map (\r -> either (throw . ParseError r) id $ parseRow r)
    . L8.lines
    <$> L8.hGetContents h


toBinary :: FilePath -> [ContractInfo] -> IO ()
toBinary file ci = withBinaryFile file WriteMode
  $ \h -> forM_ ci $ L.hPut h . runPut . put


hexToBS :: Text -> B.ByteString
hexToBS hex
  = L.toStrict $ B.toLazyByteString
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
