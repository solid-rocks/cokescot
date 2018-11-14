module Main where

import           Control.Monad (forM_, mapM_)
import           Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Text (Text)

import           System.IO (stderr)


parseLine :: L.ByteString -> Maybe (Text, Text)
parseLine l = Aeson.decode l >>= Aeson.parseMaybe parser
  where
    parser jsn = (,) <$> jsn .: "address" <*> jsn .: "bytecode"


main :: IO ()
main = do
  lines <- L.lines <$> L.getContents
  forM_ lines $ \l -> case parseLine l of
    Nothing -> do
      mapM_ (L.hPutStr stderr) ["Error in line: ", l, "\n"]
    Just (addr, code) -> return ()
