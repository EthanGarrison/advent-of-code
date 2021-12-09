
import qualified Data.Map as M
import Data.Maybe (catMaybes, maybe)

type Coord = (Int,Int)
type LineEnds = (Coord,Coord)
type HorizLine = ([Int], Int)
type VertLine = (Int, [Int])
type Line = [Coord]
type Board = M.Map Coord Int

parsePairStr :: String -> Coord
parsePairStr str = ((\[l,r] -> (l,r)) . read $ "[" ++ str ++ "]")

parseInput :: [String] -> [LineEnds]
parseInput [] = []
parseInput (curr:rest) = (extractLine . words $ curr) : parseInput rest
  where extractLine = (\[start,_,end] -> (parsePairStr start, parsePairStr end)) . (take 3)

range :: Int -> Int -> [Int]
range start end = if start >= end then reverse [end..start] else [start..end]

vertHorizLine :: LineEnds -> Maybe Line
vertHorizLine ((startX,startY),(endX,endY))
  | startX == endX = Just $ map (\y -> (startX,y)) $ range startY endY
  | startY == endY = Just $ map (\x -> (x,startY)) $ range startX endX
  | otherwise = Nothing

vertHorizDiagLine :: LineEnds -> Maybe Line
vertHorizDiagLine ends = case vertHorizLine ends of (Just l) -> Just l
                                                    Nothing -> diagCheck ends
  where
    diagCheck :: LineEnds -> Maybe Line
    diagCheck ((startX,startY),(endX,endY))
      | slopeCheck = Just $ zip (range startX endX) (range startY endY)
      | otherwise = Nothing
      where slopeCheck = (abs (endY - startY)) `mod` (abs (endX - startX)) == 0


drawLine :: Board -> Line -> Board
drawLine = foldl (insertOrUpdate)
  where
    insertOrUpdate :: Board -> (Int,Int) -> Board
    insertOrUpdate m v = M.alter (maybe (Just 1) (Just . (+ 1))) v m

part1 :: [String] -> Int
part1 = length . (filter (> 1)) . M.elems . (foldl (drawLine) M.empty) . catMaybes . (map vertHorizLine) . parseInput

part2 :: [String] -> Int
part2 = length . (filter (> 1)) . M.elems . (foldl (drawLine) M.empty) . catMaybes . (map vertHorizDiagLine) . parseInput
