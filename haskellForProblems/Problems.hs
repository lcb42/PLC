
type Order = Int
type Where = [Int]

--
--type Where = ( _ !! Int )

-- Join two tables
-- With the possibility of including 'Wheres' -> conditions which are used to filter the results.
-- Is there a way to pass in a list of wheres to be unpacked and applied as filters as explained above.
conjoin :: [[String]] -> [[String]] -> [Where] -> [[String]]
conjoin t1 t2 [] = [a ++ b | a <- t1, b <- t2]
conjoin t1 t2 wheres = [a ++ b | a <- t1, b <- t2, process a b wheres]

process :: [String] -> [String] -> [Where] -> Bool
process a b [] = True
process a b (w:ws) = (a !! (w !! 0) == b !! (w !! 1)) && (process a b ws)


eq :: ([String], Int) -> ([String], Int) -> Bool
eq (w,v) (x,y) = (w !! v) == (x !! y)


-- Take one row, rearrange to desired order
selectOne :: [Order] -> [String] -> [String]
selectOne ord xs = [ xs !! o | o <- ord]

-- Pass all rows into orderAll to be ordered
selectAll :: [Order] -> [[String]] -> [[String]]
selectAll ord xs = [selectOne ord x | x <- xs ]

-- Pass in a desired order and two lists of strings to be joined and ordered
--selectFrom :: [Order] -> [[String]] -> [[String]] -> [[String]]
--selectFrom ord xs ys = selectAll ord (conjoin xs ys)
