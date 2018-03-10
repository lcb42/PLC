import Data.List

type Order = Int
type Where = [Int]

-- TODO:Convert from AST strings to items which can be passed into the below functions

-- Join two tables and apply 'wheres if there are any'
conjoin :: [[String]] -> [[String]] -> [Where] -> [[String]]
conjoin t1 t2 [] = [a ++ b | a <- t1, b <- t2]
conjoin t1 t2 wheres = [a ++ b | a <- t1, b <- t2, process a b wheres]

-- Processes the 'wheres'
process :: [String] -> [String] -> [Where] -> Bool
process a b [] = True
process a b (w:ws) = (a !! (w !! 0) == b !! (w !! 1)) && (process a b ws)

-- Take one row, rearrange to desired order
selectOne :: [Order] -> [String] -> [String]
selectOne ord xs = [ xs !! o | o <- ord]

-- Pass all rows into orderAll to be ordered
selectAll :: [Order] -> [[String]] -> [[String]]
selectAll ord xs = [selectOne ord x | x <- xs ]

-- Pass in a desired order and two lists of strings to be joined and ordered
selectFrom :: [Order] -> [[String]] -> [[String]] -> [Where] -> [[String]]
selectFrom ord xs ys wheres = sort (selectAll ord (conjoin xs ys wheres))

readCsv :: String -> IO [[String]]
readCsv file = do contents <- readFile file
                  let lineSep = lines contents
                  let lists = splitCommaList lineSep
                  return lists 

splitCommaList :: [String] -> [[String]]
splitCommaList strings = [splitCommaOnce s | s <- strings]

splitCommaOnce :: String -> [String]
splitCommaOnce string = case dropWhile (==',') string of "" -> []
                                                         string' -> w : splitCommaOnce string''
                                                          where (w, string'') = break (==',') string'

-- Main
main = do
       readFirst <- readCsv "1a2.csv"
       readSecond <- readCsv "1b2.csv"
       let result = conjoin readFirst readSecond [] 
       print result
