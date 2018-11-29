module Main where

import           Control.Applicative
import           Control.Monad
import           Options.Applicative
import           System.IO (stdin)
import qualified ContractInfo as CI


commands :: Parser (IO ())
commands = subparser
  $ (let commandArgs = argument str
          (metavar "FILE" <> help "Write binary output to FILE")
         cmd f = CI.fromJSON stdin >>= CI.toBinary f
    in command "json-to-binary"
      $ info (cmd <$> commandArgs)
      $ progDesc
        "Convert BigQuery's dump into a compact binary representation.")
  <>
    (let commandArgs = argument str
          (metavar "FILE" <> help "Read binary format from FILE")
    in command "read-binary"
      $ info ((CI.fromBinary >=> mapM_ print) <$> commandArgs)
      $ progDesc
        "Run this command to test binary format reader.")


main :: IO ()
main = join
  $ customExecParser (prefs showHelpOnEmpty)
  $ info (commands <**> helper)
  $ fullDesc <> progDesc
    "Set of tools to analyse all the contracts\
    \ on the Ethereum blockchain."
