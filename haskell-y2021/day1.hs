
import System.IO

import Control.Applicative

import Text.Read

import Data.Maybe
import Data.Traversable (sequenceA)
import Data.List (tails)

type IncreaseState = (Int, Int)

-- Stolen from https://stackoverflow.com/a/27733778
windows :: Int -> [a] -> [[a]]
windows m = transpose' . take m . tails
  where transpose' = getZipList . sequenceA . map ZipList

incOnIncrease :: IncreaseState -> Int -> IncreaseState
incOnIncrease (acc, prev) next = (acc + (if (next > prev) then 1 else 0), next)

part1 :: [Int] -> Int
part1 (start : rest) = fst $ foldl (incOnIncrease) (0,start) rest

part2 :: [Int] -> Int
part2 xs = part1 $ (map sum) . (windows 3) $ xs

parseInput :: String -> IO [Int]
parseInput file = do
  content <- lines <$> readFile file
  return $ catMaybes $ map readMaybe $ content

