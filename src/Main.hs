module Main where

import           Control.Applicative
import           Control.Monad (join)
import           Options.Applicative
import           BinaryFormat (jsonToBinary)


commands :: Parser (IO ())
commands = subparser
  $ let commandArgs = strOption
          $  long "output" <> short 'o'
          <> metavar "FILE"
          <> help "Write binary output to FILE"
    in command "json-to-binary"
      $ info (jsonToBinary <$> commandArgs)
      $ progDesc
        "Convert BigQuery's dump into a compact binary representation."


main :: IO ()
main = join
  $ customExecParser (prefs showHelpOnEmpty)
  $ info (commands <**> helper)
  $ fullDesc <> progDesc
    "Set of tools to analyse all the contracts\
    \ on the Ethereum blockchain."
