module Main where
import Tokens
import Grammar
import Data.List
import System.Environment

type Order = String
type WherePair = (String, String)
type VarMap = (String, Int)

-- Join two tables
conjoin :: [[String]] -> [[String]] -> [[String]]
conjoin t1 t2 = [a ++ b | a <- t1, b <- t2]

-- Locate the 'where' clauses of the statement
createWherePairs :: Exp -> [WherePair]
createWherePairs (TakeFromWhere x y z) = getPairs z
createWherePairs (TakeFrom x y) = []

-- Create a list of all of the pairs of variables which should be equal, as per the 'where' clauses
getPairs :: Where -> [WherePair]
getPairs (Eq x) = [x]
getPairs (And x y) = getPairs x ++ getPairs y

-- Apply wheres after conjoin
applyWhere :: [[String]] -> [WherePair] -> [[String]]
applyWhere table wheres = [row | row <- table, rowBelongs row wheres]

rowBelongs :: [String] -> [WherePair] -> Bool
rowBelongs row [] = True
rowBelongs row (w:wheres) = (row !! (read (fst w)) == row !! (read (snd w))) && rowBelongs row wheres

-- Take one row, rearrange to desired order
selectOne :: [Order] -> [String] -> [String]
selectOne ord xs = [ xs !! (read o ::Int) | o <- ord]

-- Pass all rows into orderAll to be ordered
selectAll :: [Order] -> [[String]] -> [[String]]
selectAll ord xs = [selectOne ord x | x <- xs ]

-- Retrieve the desired order of variables from the 'take' clause
getOrder :: Exp -> [Order]
getOrder (TakeFromWhere x y z) = x
getOrder (TakeFrom x y) = x

-- Read in the csv, split by newline and comma
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

readTables :: Exp -> IO [[String]]
readTables (TakeFromWhere x y z) = readTable y
readTables (TakeFrom x y) = readTable y

-- Read in all the csvs in the query and store each as a separate list
readTable :: File -> IO [[String]]
readTable (File name vars) = do file <- readCsv (name ++ ".csv")
                                return (file)

readTable (Conjoin file1 file2) = do file1 <- readTable file1
                                     file2 <- readTable file2
                                     let conjoined = conjoin file1 file2
                                     return (conjoined)

-- Create a list of associations between variable names and column indexes
getAssocs :: Exp -> [VarMap]
getAssocs (TakeFromWhere x y z) = zip (getVars y) [0..]
getAssocs (TakeFrom x y) = zip (getVars y) [0..]

-- Retrieve all the variables in the tables
getVars :: File -> [String]
getVars (File name vars) = vars
getVars (Conjoin file1 file2) = getVars file1 ++ getVars file2

-- Replace all instances of string variables with the index of the column they represent
substituteExp :: Exp -> [VarMap] -> Exp
substituteExp (TakeFromWhere x y z) assocs = TakeFromWhere (substituteStrings x assocs) (substituteFile y assocs) (substituteWheres z assocs)
substituteExp (TakeFrom x y) assocs = TakeFrom (substituteStrings x assocs) (substituteFile y assocs)

substituteFile :: File -> [VarMap] -> File
substituteFile (File name vars) assocs = File name (substituteStrings vars assocs)
substituteFile (Conjoin file1 file2) assocs = Conjoin (substituteFile file1 assocs) (substituteFile file2 assocs)

substituteStrings :: [String] -> [VarMap] -> [String]
substituteStrings strings assocs = [ show index | s <- strings, (name,index) <- assocs, s == name ]

-- TODO: THROW AN ERROR
substituteString :: String -> [VarMap] -> String
substituteString string assocs = show $ head [ snd a | a <- assocs, fst a == string]

substituteWheres :: Where -> [VarMap] -> Where
substituteWheres (Eq x) assocs = Eq (substituteString (fst x) assocs , substituteString (snd x) assocs)
substituteWheres (And x y) assocs = And (substituteWheres x assocs) (substituteWheres y assocs)

-- Take a list of lists and return each list as a string on a newline
formatOutput :: [[String]] -> String
formatOutput [] = []
formatOutput (s:strings) = mergeList s ++ "\n" ++ formatOutput strings

mergeList :: [String] -> String
mergeList [] = []
mergeList (s:[]) = s
mergeList (s:string) = s ++ "," ++ mergeList string

main = do
 args <- getArgs
 progString <- readFile (head args)
 let ast = parseCql $ alexScanTokens progString
 let assocs = getAssocs ast
 let subbedAst = substituteExp ast assocs
 conjoinedTable <- readTables subbedAst
 let wheres = createWherePairs subbedAst
 let wheresApplied = applyWhere conjoinedTable wheres
 let inOrder = selectAll (getOrder subbedAst) wheresApplied
 putStr (formatOutput (sort inOrder))
