module Main where

import           Options.Applicative
import           System.IO (stdin)
import qualified ContractInfo as CI
import qualified Analysis as A


data Options = Options

options :: Parser Options
options = pure Options

main :: IO ()
main = do
  Options <- customExecParser (prefs showHelpOnEmpty)
      $ info (options <**> helper)
      $ fullDesc <> progDesc
        "Set of tools to analyse all the contracts\
        \ on the Ethereum blockchain."

  ci <- CI.fromText stdin
  A.printStats ci
