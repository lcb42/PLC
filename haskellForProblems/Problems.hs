module Main where
import Tokens
import Grammar
import Data.List
import System.Environment

type Order = String
type VarMap = (String, Int)

main = do
 args <- getArguments
 progString <- readFile (head args)
 let ast = parseCql $ alexScanTokens progString
 let assocs = getAssocs ast
 let subbedAst = substituteExp ast assocs
 conjoinedTable <- readTables subbedAst
 let wheresApplied = applyWheres conjoinedTable subbedAst
 let inOrder = selectAll (getOrder subbedAst) wheresApplied
 putStr (formatOutput (sort inOrder))

getArguments :: IO [String]
getArguments = do args <- getArgs
                  case args of [] -> error (".cql program file required to run query")
                               [x] -> return args
                               _ -> error ("More than one argument supplied. One .cql program file required")


-- CREATE VARIABLE-COlUMN INDEX ASSOCIATIONS FROM THE FILES
-- Create a list of associations between variable names and column indexes
getAssocs :: Exp -> [VarMap]
getAssocs (TakeFromWhere x y z)
 | (getDuplicate (sort (getVars y))) /= [] = error (getDuplicate (sort (getVars y)) ++ " is used to name more than one variable.")
 | otherwise = zip (getVars y) [0..]
getAssocs (TakeFrom x y)
 | (getDuplicate (sort (getVars y))) /= [] = error (getDuplicate (sort (getVars y)) ++ " is used to name more than one variable.")
 | otherwise = zip (getVars y) [0..]

-- Find if any variables were used to name more than one column of the files
getDuplicate :: [String] -> String
getDuplicate [] = []
getDuplicate (x:[]) = []
getDuplicate (x:y:xs)
 | x == y = x
 | otherwise = getDuplicate (y:xs)

-- Retrieve all the variables listed in files eg. A(var1,var2)
getVars :: File -> [String]
getVars (File name vars) = vars
getVars (Conjoin file1 file2) = getVars file1 ++ getVars file2


-- SUBSTITUTE VARIABLES IN THE AST WITH THEIR COLUMN NUMBERS
-- Replace all instances of string variables with the index of the column they represent
substituteExp :: Exp -> [VarMap] -> Exp
substituteExp (TakeFromWhere x y z) assocs = TakeFromWhere (substituteStrings x assocs) (substituteFile y assocs) (substituteWheres z assocs)
substituteExp (TakeFrom x y) assocs = TakeFrom (substituteStrings x assocs) (substituteFile y assocs)

substituteFile :: File -> [VarMap] -> File
substituteFile (File name vars) assocs = File name (substituteStrings vars assocs)
substituteFile (Conjoin file1 file2) assocs = Conjoin (substituteFile file1 assocs) (substituteFile file2 assocs)

substituteStrings :: [String] -> [VarMap] -> [String]
substituteStrings ["ALL"] assocs = ["ALL"]
substituteStrings [] assocs = []
substituteStrings (s:strings) assocs = case (getIndex s assocs) of [] -> error ("No such variable " ++ s)
                                                                   _ -> getIndex s assocs ++ substituteStrings strings assocs
 where getIndex s assocs = [show index | (name,index) <- assocs, s == name]

substituteString :: String -> [VarMap] -> String
substituteString string assocs 
 | getMatch string assocs == [] = error ("No such variable " ++ string)
 | otherwise = show (head (getMatch string assocs))
  where getMatch string assocs = [ snd a | a <- assocs, fst a == string]

-- If column comparisons, replace variables with the column index. If literals, leave them as strings.
substituteWheres :: Where -> [VarMap] -> Where
substituteWheres (Eq x) assocs = Eq (substituteString (fst x) assocs , substituteString (snd x) assocs)
substituteWheres (NotEq x) assocs = NotEq (substituteString (fst x) assocs , substituteString (snd x) assocs)
substituteWheres (Gt x) assocs = Gt (substituteString (fst x) assocs , substituteString (snd x) assocs)
substituteWheres (GtEq x) assocs = GtEq (substituteString (fst x) assocs , substituteString (snd x) assocs)
substituteWheres (Lt x) assocs = Lt (substituteString (fst x) assocs , substituteString (snd x) assocs)
substituteWheres (LtEq x) assocs = LtEq (substituteString (fst x) assocs , substituteString (snd x) assocs)
substituteWheres (And x y) assocs = And (substituteWheres x assocs) (substituteWheres y assocs)
substituteWheres (EqLit x) assocs = EqLit (substituteString (fst x) assocs, snd x)
substituteWheres (GtLit x) assocs = GtLit (substituteString (fst x) assocs, snd x)
substituteWheres (LtLit x) assocs = LtLit (substituteString (fst x) assocs, snd x)
substituteWheres (NotEqLit x) assocs = NotEqLit (substituteString (fst x) assocs, snd x)
substituteWheres (GtEqLit x) assocs = GtEqLit (substituteString (fst x) assocs, snd x)
substituteWheres (LtEqLit x) assocs = LtEqLit (substituteString (fst x) assocs, snd x)


-- READ IN THE TABLES AND CONJOIN IF SPECIFIED
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

-- Read in the tables from the 'from' clause
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

-- Join two tables
conjoin :: [[String]] -> [[String]] -> [[String]]
conjoin t1 t2 = [a ++ b | a <- t1, b <- t2]


-- FILTER THE TABLE ACCORDING TO WHERE CLAUSES
-- Apply all where clauses to a table
applyWheres :: [[String]] -> Exp -> [[String]]
applyWheres table (TakeFromWhere x y z) = applyWhere table z
applyWheres table (TakeFrom x y) = table

-- Go through each row and test all where clauses, return if all apply
applyWhere :: [[String]] -> Where -> [[String]]
applyWhere table wheres = [row | row <- table, rowBelongs row wheres]

-- Test whether a given row satisfies all the where clauses
rowBelongs :: [String] -> Where -> Bool
rowBelongs row (Eq x) = (row !! (read (fst x))) == (row !! (read (snd x)))
rowBelongs row (NotEq x) = (row !! (read (fst x))) /= (row !! (read (snd x)))
rowBelongs row (Gt x) = (row !! (read (fst x))) > (row !! (read (snd x)))
rowBelongs row (GtEq x) = (row !! (read (fst x))) >= (row !! (read (snd x)))
rowBelongs row (Lt x) = (row !! (read (fst x))) < (row !! (read (snd x)))
rowBelongs row (LtEq x) = (row !! (read (fst x))) <= (row !! (read (snd x)))
rowBelongs row (And x y) = rowBelongs row x && rowBelongs row y
rowBelongs row (EqLit x) = row !! (read (fst x)) == snd x
rowBelongs row (GtLit x) = row !! (read (fst x)) > snd x
rowBelongs row (LtLit x) = row !! (read (fst x)) < snd x
rowBelongs row (NotEqLit x) = row !! (read (fst x)) /= snd x
rowBelongs row (GtEqLit x) = row !! (read (fst x)) >= snd x
rowBelongs row (LtEqLit x) = row !! (read (fst x)) <= snd x


-- ORDER THE COLUMNS AS DEFINED IN THE FETCH CLAUSE
-- Retrieve the desired order of variables from the 'take' clause
getOrder :: Exp -> [Order]
getOrder (TakeFromWhere x y z) = x
getOrder (TakeFrom x y) = x

-- Pass all rows and rearranged into desired order
selectAll :: [Order] -> [[String]] -> [[String]]
selectAll ["ALL"] xs = xs
selectAll ord xs = [selectOne ord x | x <- xs ]

-- Take one row, rearrange to desired order
selectOne :: [Order] -> [String] -> [String]
selectOne ord xs = [ xs !! (read o ::Int) | o <- ord]


-- FORMAT THE FINAL OUTPUT
-- Take a list of lists and return each list as a string on a newline
formatOutput :: [[String]] -> String
formatOutput [] = []
formatOutput (s:strings) = mergeList s ++ "\n" ++ formatOutput strings

mergeList :: [String] -> String
mergeList [] = []
mergeList (s:[]) = s
mergeList (s:string) = s ++ "," ++ mergeList string
