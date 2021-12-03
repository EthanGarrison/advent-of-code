
-- part2 (\(zero,one) -> if zero > one then '0' else '1') inputData
-- part2 (\(zero,one) -> if zero <= one then '0' else '1') inputData
-- reverse the output and pass to `convertResultStr` for answer
part2 :: ((Int,Int) -> Char) -> [String] -> String
part2 _ [] = ""
part2 _ [_str] = _str
part2 sigDecide xs = rec xs ""
  where rec :: [String] -> String -> String
        rec [] res = reverse res
        rec [_str] res = _str
        rec xs res 
          | (length $ head xs) == length res = reverse res
          | otherwise = let currIdx = length res
                            currOcc = (countBitOccurance xs) !! currIdx
                            sigChar = sigDecide currOcc 
                        in rec (filter (\str -> (str !! currIdx) == sigChar) xs) (sigChar : res)

convertResultStr :: (Enum a, Floating a) => String -> a
convertResultStr str = foldl (\acc (i,c) -> acc + (if c == '1' then 2 ** i else 0)) 0 $ zip [0..] str

countBitOccurance :: [String] -> [(Int,Int)]
countBitOccurance xs = foldl updateCache initialCache xs
  where width = length $ head xs
        initialCache = replicate width (0,0)
        incCache :: ((Int,Int), Char) -> (Int,Int)
        incCache ((zero,one),c) = if c == '0' then (zero + 1, one) else (zero, one + 1)
        updateCache :: [(Int,Int)] -> String -> [(Int,Int)]
        updateCache xxs str = (map incCache) $ zip xxs str

part1 :: [String] -> String
part1 = cacheToString . countBitOccurance
  where cacheToString = foldl (\acc (zero,one) -> (if zero > one then '0' else '1') : acc) ""
               
