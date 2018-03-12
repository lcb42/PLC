module Main where
import Tokens
import Grammar
import Data.List

type Order = Int
--type Where = [Int]
type WherePair = (Int, Int)
type VarMap = (String, Int)

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

getFileNames :: Exp -> [(String,[String])]
getFileNames (TakeFromWhere x y z) = retrieveFile y

retrieveFile :: File -> [(String,[String])]
retrieveFile (File name vars) = [(name,vars)]
retrieveFile (Conjoin first second) = retrieveFile first ++ retrieveFile second

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

--eval :: Exp -> String
--eval (TakeFromWhere x y z) = readTable y

--readTable :: File -> [[[String]]]
readTable (File name vars) = do file <- readCsv (name ++ ".csv")
                                return ([file])



readTable (Conjoin file1 file2) = do file1 <- readTable file1
                                     file2 <- readTable file2
                                     return (file1 ++ file2)

-- Create a list of associations between variable names and column indexes
getAssocs :: Exp -> [VarMap]
getAssocs (TakeFromWhere x y z) = zip (getVars y) [0..]

-- Retrieve all the variables in the tables
getVars :: File -> [String]
getVars (File name vars) = vars
getVars (Conjoin file1 file2) = getVars file1 ++ getVars file2


-- Main
--main = do
--       readFirst <- readCsv "5a3.csv"
--       readSecond <- readCsv "5b3.csv"
--       let result = conjoin readFirst readSecond
--       let filtered = applyWhere result [(1,2)]
--       let final = sort $ selectAll [0,3] filtered       
--       print $ formatOutput final

main = do
 progString <- readFile "program.txt" 
 let ast = parseCql $ alexScanTokens progString
 let assocs = getAssocs ast
 
 print ast


