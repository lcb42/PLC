
type Order = Int

--
type Where = ( _ !! Int )

-- Join two tables
-- With the possibility of including 'Wheres' -> conditions which are used to filter the results.
-- Is there a way to pass in a list of wheres to be unpacked and applied as filters as explained above.
--conjoin :: [[String]] -> [[String]] -> [[String]]
conjoin t1 t2 [] = [a ++ b | a <- t1, b <- t2]
conjoin t1 t2 wheres = [a ++ b | a <- t1, b <- t2, wheres]

-- Take one row, rearrange to desired order
orderOne :: [Order] -> [String] -> [String]
orderOne ord xs = [ xs !! o | o <- ord]

-- Pass all rows into orderAll to be ordered
orderAll :: [Order] -> [[String]] -> [[String]]
orderAll ord xs = [selectOne ord x | x <- xs ]

-- Pass in a desired order and two lists of strings to be joined and ordered
selectFrom :: [Order] -> [[String]] -> [[String]] -> [[String]]
selectFrom ord xs ys = selectAll ord (conjoin xs ys)
