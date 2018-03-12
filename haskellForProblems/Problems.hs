module Main where
import Tokens
import Grammar
import Data.List

type Order = String
type WherePair = (String, String)
type VarMap = (String, Int)

-- Join two tables and apply 'wheres if there are any'
conjoin :: [[String]] -> [[String]] -> [[String]]
conjoin t1 t2 = [a ++ b | a <- t1, b <- t2]

createWherePairs :: Exp -> [WherePair]
createWherePairs (TakeFromWhere x y z) = getPairs z
createWherePairs (TakeFrom x y) = []

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

getOrder :: Exp -> [Order]
getOrder (TakeFromWhere x y z) = x
getOrder (TakeFrom x y) = x

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
getFileNames (TakeFrom x y) = retrieveFile y

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


main = do
 progString <- readFile "program.txt"
 let ast = parseCql $ alexScanTokens progString
 let assocs = getAssocs ast
 let subbedAst = substituteExp ast assocs
 conjoinedTable <- readTables subbedAst
 let wheres = createWherePairs subbedAst
 let wheresApplied = applyWhere conjoinedTable wheres
 let inOrder = selectAll (getOrder subbedAst) wheresApplied
 print inOrder
