module Main where
import Tokens
import Grammar
import Data.List

type Order = Int
--type Where = [Int]
type WherePair = (Int, Int)

-- TODO:Convert from AST strings to items which can be passed into the below functions

-- Join two tables and apply 'wheres if there are any'
conjoin :: [[String]] -> [[String]] -> [[String]]
conjoin t1 t2 = [a ++ b | a <- t1, b <- t2]

-- Apply wheres after conjoin
applyWhere :: [[String]] -> [WherePair] -> [[String]]
applyWhere table wheres = [row | row <- table, rowBelongs row wheres]

rowBelongs :: [String] -> [WherePair] -> Bool
rowBelongs row [] = True
rowBelongs row (w:wheres) = (row !! (fst w) == row !! (snd w)) && rowBelongs row wheres

-- Take one row, rearrange to desired order
selectOne :: [Order] -> [String] -> [String]
selectOne ord xs = [ xs !! o | o <- ord]

-- Pass all rows into orderAll to be ordered
selectAll :: [Order] -> [[String]] -> [[String]]
selectAll ord xs = [selectOne ord x | x <- xs ]

-- Take a list of lists and return each list as a string on a newline
formatOutput :: [[String]] -> String
formatOutput [] = []
formatOutput (s:[]) = mergeList s
formatOutput (s:strings) = mergeList s ++ "\n" ++ formatOutput strings

mergeList :: [String] -> String
mergeList [] = []
mergeList (s:[]) = s
mergeList (s:string) = s ++ "," ++ mergeList string

-- Pass in a desired order and two lists of strings to be joined and ordered
selectFrom :: [Order] -> [[String]] -> [[String]] -> [[String]]
selectFrom ord xs ys = sort (selectAll ord (conjoin xs ys))

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
--main = do
--       readFirst <- readCsv "5a3.csv"
--       readSecond <- readCsv "5b3.csv"
--       let result = conjoin readFirst readSecond
--       let filtered = applyWhere result [(1,2)]
--       let final = sort $ selectAll [0,3] filtered       
--       print $ formatOutput final

main = do
 s <- readFile "text.txt" 
 let tokens = alexScanTokens s
 let ast = parseCql tokens
 print ast
