joinTables :: [[String]] -> [[String]] -> [[String]]
joinTables t1 t2 = [a ++ b | a <- t1, b <- t2]

type Order = Int

getOrder :: [Order] -> [[String]] -> [String]
getOrder order list = [ l !! o | l <- list, o <- order]

--compileLists :: [Order] -> [[String]] -> [[String]]
compileLists order [] = []
compileLists order (x:xs) = [getOrder order x] ++ compileLists order xs

orderList order xs = [ x !! o | x <- xs, o <- order]
