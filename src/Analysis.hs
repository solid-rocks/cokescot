module Analysis where

import           Data.Ord (comparing)
import           Data.List (foldl', sortBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed as U

import           ContractInfo


contractMap :: [ContractInfo] -> Map Code [Address]
contractMap = foldl' f Map.empty
  where
    f m ContractInfo{..} = Map.insertWith (++) code [addr] m

newtype Len = Len Int deriving Show
newtype Count = Count Int deriving Show

printStats :: [ContractInfo] -> IO ()
printStats ci = do
  let ciMap = contractMap ci
  print $ ("# different contracts", Map.size ciMap)
  print $ ("# empty contracts", length $ ciMap Map.! U.empty)
  putStrLn "Sizes of Top 50 most common contracts"
  let codesAndCounts = Map.toList $ Map.map length ciMap
  let histogramByCount = sortBy (flip $ comparing snd) codesAndCounts
  mapM_ print $ take 50
    [ (Count count, Len $ U.length code)
    | (code, count) <- histogramByCount
    ]
  putStrLn "Sizes of Top 50 biggest contracts"
  let histogramByLen = sortBy (flip $ comparing fst)
        $ map (\(code, count) -> (U.length code, count)) codesAndCounts

  mapM_ print $ take 50
    [ (Count count, Len len)
    | (len, count) <- histogramByLen
    ]
