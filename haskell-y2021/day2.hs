
import Data.Char (toUpper)
import Text.Read (readMaybe)

data Direction = Forward | Up | Down deriving (Show, Read)
type SubControl = (Direction, Int)
type SubLocation = (Int, Int)
type SubLocationAim = (Int, Int, Int)

parseDirection :: String -> Maybe Direction
parseDirection (c : rest) = readMaybe (toUpper c : rest)

parseSubControl :: [String] -> Maybe SubControl
parseSubControl (_dir : _dist : _) = do
  dir <- parseDirection _dir
  dist <- readMaybe _dist
  return (dir, dist)
parseSubControl _ = Nothing

part1 :: [SubControl] -> SubLocation
part1 = foldl computeMove (0,0)
  where
    computeMove :: SubLocation -> SubControl -> SubLocation
    computeMove (horiz, depth) (dir, dist) =
      case dir of Forward -> (horiz + dist, depth)
                  Up -> (horiz, depth - dist)
                  Down -> (horiz, depth + dist)

part2 :: [SubControl] -> SubLocationAim
part2 = foldl computeMove (0,0,0)
  where
    computeMove :: SubLocationAim -> SubControl -> SubLocationAim
    computeMove (horiz, depth, aim) (dir, dist) =
      case dir of Forward -> (horiz + dist, depth + dist * aim, aim)
                  Up -> (horiz, depth, aim - dist)
                  Down -> (horiz, depth, aim + dist)


