module Main where

import           Control.Applicative
import           Control.Monad
import           Options.Applicative
import           BinaryFormat (jsonToBinary, readBinary)


commands :: Parser (IO ())
commands = subparser
  $ (let commandArgs = argument str
          (metavar "FILE" <> help "Write binary output to FILE")
    in command "json-to-binary"
      $ info (jsonToBinary <$> commandArgs)
      $ progDesc
        "Convert BigQuery's dump into a compact binary representation.")
  <>
    (let commandArgs = argument str
          (metavar "FILE" <> help "Read binary format from FILE")
    in command "read-binary"
      $ info ((readBinary >=> mapM_ print) <$> commandArgs)
      $ progDesc
        "Run this command to test binary format reader.")


main :: IO ()
main = join
  $ customExecParser (prefs showHelpOnEmpty)
  $ info (commands <**> helper)
  $ fullDesc <> progDesc
    "Set of tools to analyse all the contracts\
    \ on the Ethereum blockchain."
