
import Data.List (find)
import Data.Maybe (isJust,isNothing)
import qualified Data.IntMap.Strict as M

type Coord = (Int,Int)
type Board = M.IntMap Coord
type BoardStr = [[String]]
type CoordCnt = M.IntMap Int

-- All code assumes boards are 5x5

chunks :: Int -> [a] -> [[a]]
chunks i [] = []
chunks 1 xs = map (\x -> [x]) xs
chunks i xs | length xs < i = [xs]
            | otherwise = (take i xs) : chunks i (drop i xs)

parseInput :: [String] -> ([Int],[BoardStr])
parseInput (s:xs) = (read $ "[" ++ s ++ "]", map (map words) $ map tail $ chunks 6 xs)

parseBoardStr :: BoardStr -> Board
parseBoardStr = reduceBoardStr . (zip [0..]) . (map (zip [0..]))
  where reduceBoardStr = (foldl (\board (rowIdx,row) -> 
                           foldl (\_b (colIdx,value) -> 
                             M.insert (read value) (rowIdx,colIdx) _b) board row)) M.empty

countAndDrop :: Int -> [(CoordCnt,Board)] -> [(CoordCnt,Board)]
countAndDrop f = map check
  where check (cnt,board) =
          case (M.lookup f board) of Nothing -> (cnt,board)
                                     (Just coord) -> (incrementOrInsertCoord coord cnt, M.delete f board)

incrementOrInsertCoord :: Coord -> CoordCnt -> CoordCnt
incrementOrInsertCoord (row,col) cnt = M.alter incOrInsert row $ M.alter incOrInsert (col + 5) cnt
  where incOrInsert :: Maybe Int -> Maybe Int
        incOrInsert (Just x) = Just (x + 1)
        incOrInsert Nothing = Just 1

bingoCheck :: [(CoordCnt, Board)] -> Maybe Board
bingoCheck = (fmap snd) . (find (\(cnt,_) -> cntCheck cnt))
  where cntCheck :: CoordCnt -> Bool
        cntCheck = isJust . (find ((==) 5)) . M.elems

bingoFilter :: [(CoordCnt, Board)] -> [(CoordCnt,Board)]
bingoFilter = filter (\(cnt,_) -> cntCheck cnt)
  where cntCheck :: CoordCnt -> Bool
        cntCheck = isNothing . (find ((==) 5)) . M.elems

part1 :: [String] -> (Int, Board)
part1 input = rec rndNums (map (\x -> (M.empty,x)) boards)
  where 
    (rndNums, boardStrs) = parseInput input
    boards = map parseBoardStr boardStrs
    rec :: [Int] -> [(CoordCnt, Board)] -> (Int, Board)
    rec [] _ = (0,M.empty)
    rec (currNum:rest) _boards = let newBoards = countAndDrop currNum _boards
                                 in case bingoCheck newBoards of (Just _b) -> (currNum,_b)
                                                                 Nothing -> rec rest newBoards

part2 :: [String] -> (Int, Board)
part2 input = rec rndNums (map (\x -> (M.empty,x)) boards)
  where
    (rndNums, boardStrs) = parseInput input
    boards = map parseBoardStr boardStrs
    rec :: [Int] -> [(CoordCnt, Board)] -> (Int, Board)
    rec [] _ = (0,M.empty)
    rec (currNum:rest) [board] = let newBoard = countAndDrop currNum [board]
                                 in case bingoCheck newBoard of (Just _b) -> (currNum,_b)
                                                                Nothing -> rec rest newBoard
    rec (currNum:rest) _boards = let newBoards = countAndDrop currNum _boards
                                     filtered = bingoFilter newBoards
                                 in rec rest filtered
