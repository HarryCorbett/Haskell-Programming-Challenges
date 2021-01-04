{-# LANGUAGE DeriveGeneric #-}
-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2020
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return an arbitrary value that is usually wrong 

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenges (WordSearchGrid,Placement,Posn,Orientation(..),solveWordSearch, createWordSearch,
    LamMacroExpr(..),LamExpr(..),prettyPrint, parseLamMacro,
    cpsTransform,innerRedn1,outerRedn1,compareInnerOuter) where

-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
-- We import System.Random - make sure that your installation has it installed - use stack ghci and stack ghc
import Data.Char
import Parsing
import Control.Monad
import Data.List
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import System.IO
import System.Random
import Data.Maybe
import Data.Function
import Data.Functor


-- types for Part I
type WordSearchGrid = [[ Char ]]
type Placement = (Posn,Orientation)
type Posn = (Int,Int)
data Orientation = Forward | Back | Up | Down | UpForward | UpBack | DownForward | DownBack deriving (Eq,Ord,Show,Read)

-- types for Parts II and III
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr  |
               LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read)

-- END OF CODE YOU MUST NOT MODIFY

-- ADD YOUR OWN CODE HERE

-- Challenge 1 --

--Search forward for a given string at a given position in the grid
searchForward :: String -> WordSearchGrid -> Posn -> Bool
searchForward [] grid pos = True
searchForward (x:xs) grid (c,r)  | c < length (grid !! 1) && (grid !! r !! c) == x = searchForward xs grid (c+1,r)
                                 | otherwise = False

--Search backwards for a given string at a given position in the grid
searchBack :: String -> WordSearchGrid -> Posn -> Bool
searchBack [] grid pos = True
searchBack (x:xs) grid (c,r)  | c > -1 && (grid !! r !! c) == x = searchBack xs grid (c-1,r)
                              | otherwise = False

--Search up for a given string at a given position in the grid
searchUp :: String -> WordSearchGrid -> Posn -> Bool
searchUp [] grid pos = True
searchUp (x:xs) grid (c,r)  | r > -1 && (grid !! r !! c) == x = searchUp xs grid (c,r-1)
                            | otherwise = False

--Search down for a given string at a given position in the grid
searchDown :: String -> WordSearchGrid -> Posn -> Bool
searchDown [] grid pos = True
searchDown (x:xs) grid (c,r)  | r < length grid && (grid !! r !! c) == x = searchDown xs grid (c,r+1)
                              | otherwise = False

--Search diagonally up and forward for a given string at a given position in the grid
searchUpForward :: String -> WordSearchGrid -> Posn -> Bool
searchUpForward [] grid pos = True
searchUpForward (x:xs) grid (c,r)  | c < length (grid !! 1) && r > -1 && (grid !! r !! c) == x = searchUpForward xs grid (c+1,r-1)
                                   | otherwise = False

--Search diagonally up and backwards for a given string at a given position in the grid
searchUpBack :: String -> WordSearchGrid -> Posn -> Bool
searchUpBack [] grid pos = True
searchUpBack (x:xs) grid (c,r)  | c > -1 && r > -1 && (grid !! r !! c) == x = searchUpBack xs grid (c-1,r-1)
                                | otherwise = False

--Search diagonally down and forward for a given string at a given position in the grid
searchDownForward :: String -> WordSearchGrid -> Posn -> Bool
searchDownForward [] grid pos = True
searchDownForward (x:xs) grid (c,r)  | r < length grid && c < length (grid !! 1) && (grid !! r !! c) == x = searchDownForward xs grid (c+1,r+1)
                                     | otherwise = False

--Search diagonally down and backwards for a given string at a given position in the grid
searchDownBack :: String -> WordSearchGrid -> Posn -> Bool
searchDownBack [] grid pos = True
searchDownBack (x:xs) grid (c,r)  | r < length grid && c > -1 && (grid !! r !! c) == x = searchDownBack xs grid (c-1,r+1)
                                  | otherwise = False

--Search for a given word in all directions from a position in the grid
searchForWordAtPosition :: String -> WordSearchGrid -> Posn -> (String, Maybe Placement)
searchForWordAtPosition string grid position | searchForward string grid position = (string, Just(position, Forward))
                                             | searchBack string grid position = (string, Just(position, Back))
                                             | searchUp string grid position = (string, Just(position, Up))
                                             | searchDown string grid position = (string, Just(position, Down))
                                             | searchUpForward string grid position = (string, Just(position, UpForward))
                                             | searchUpBack string grid position = (string, Just(position, UpBack))
                                             | searchDownForward string grid position = (string, Just(position, DownForward))
                                             | searchDownBack string grid position = (string, Just(position, DownBack))
                                             | otherwise = (string, Nothing)

--Ensures only one Nothing tuple is in the output for each string that is not found, and that for found strings, only the tuple containing its position is in the output
--Initially there is a value for every position that each word is checked in, this reduces it to one tuple per string, keeping the correct information
removeNothingForFoundWord :: String -> [(String,Maybe Placement)] -> [(String,Maybe Placement)]
removeNothingForFoundWord string xs | null [ x | x <- xs , isJust (snd x) ] = [(string,Nothing)]
                                    | otherwise = [ x | x <- xs , isJust (snd x) ]

--Search for a string at all posiitons that contain the strings starting character
searchForWord :: String -> WordSearchGrid -> [(String,Maybe Placement)]
searchForWord [] _        = []
searchForWord string grid = removeNothingForFoundWord string [ searchForWordAtPosition string grid z | z <- [ (x,y) | (y,line) <- zip [0..] grid, x <- elemIndices (head string) line ] ]

--Recursively call the search for each word and collect the results into a list
solveWordSearch :: [ String ] -> WordSearchGrid -> [ (String,Maybe Placement) ]
solveWordSearch _ [[]]      = error "given grid is empty"
solveWordSearch [] _        = []
solveWordSearch (x:xs) grid = searchForWord x grid ++ solveWordSearch xs grid


-- Two examples for you to try out, the first of which is in the instructions

exGrid1'1 = [ "HAGNIRTSH" , "SACAGETAK", "GCSTACKEL","MGHKMILKI","EKNLETGCN","TNIRTLETE","IRAAHCLSR","MAMROSAGD","GIZKDDNRG" ] 
exWords1'1 = [ "HASKELL","STRING","STACK","MAIN","METHOD"]

exGrid1'2 = ["ROBREUMBR","AURPEPSAN","UNLALMSEE","YGAUNPYYP","NLMNBGENA","NBLEALEOR","ALRYPBBLG","NREPBEBEP","YGAYAROMR"]
exWords1'2 = [ "BANANA", "ORANGE", "MELON", "RASPBERRY","APPLE","PLUM","GRAPE" ]


-- Challenge 2 --

--Removes duplicate characters from character list
removeDuplicates :: Eq a => [a] -> [a] -> [a]
removeDuplicates [] _ = []
removeDuplicates (x:xs) list | x `elem` list = removeDuplicates xs list
                             | otherwise = x : removeDuplicates xs (x:list)

--Gets the list of unique characters that will be used to fill spaces in the word search
getCharacterList :: [String] -> [Char]
getCharacterList [] = []
getCharacterList xs = map toUpper (removeDuplicates (intercalate "" xs ) [])

--Calculate the size of the grid based on the given density
calculateGridSize :: Integral b => [String] -> Double -> b
calculateGridSize strings density = floor(sqrt (fromIntegral (length (intercalate "" strings)) / density))

--Choose a random value from the given list
chooseRandomFromList :: [a] -> IO a
chooseRandomFromList xs = randomRIO (0, length xs - 1) <&> (xs !!)

-- Create a grid of consisting of only '-' of size n x n
makeEmptyGrid :: Int -> WordSearchGrid
makeEmptyGrid n = replicate n (replicate n '-')

--Insert a single word at random position in a random orientation
--If the position/orientation cannot fit the word, keep calling the function until a valid position/orientation is found
insertWord :: WordSearchGrid -> String -> IO WordSearchGrid
insertWord grid word = do wordInsertion <- chooseRandomFromList [(checkForward,insertForward),(checkBack,insertBack),(checkUp,insertUp),(checkDown,insertDown),(checkForwardDown,insertForwardDown),(checkBackUp,insertBackUp),(checkForwardUp,insertForwardUp),(checkBackDown,insertBackDown)]
                          col <- randomRIO (0, length (head grid) - 1) 
                          row <- randomRIO (0, length grid -1)
                          if fst wordInsertion grid word (col,row)
                            then return $ snd wordInsertion grid word (col,row)
                          else insertWord grid word

--Call insertWord for all words in the input list of words
insertWords :: WordSearchGrid -> [String] -> IO WordSearchGrid
insertWords = foldM insertWord

--Checks if a word can fit forward at a given position
checkForward :: WordSearchGrid -> String -> Posn -> Bool
checkForward grid [] (col,row) = True
checkForward grid (x:xs) (col,row) | col < length (grid !! 1) && (grid !! row !! col) == '-' = checkForward grid xs (col+1,row)
                                   | otherwise = False

--Insert the word forward (only called after checkFoward has returned true to ensure the word fits)
insertForward :: WordSearchGrid -> String -> Posn -> WordSearchGrid
insertForward grid word (col,row) = take row grid ++ (take col (grid !! row) ++ word ++ drop (length word + col) (grid !! row)) : drop (row+1) grid 

--Checks if a word can fit backwards at a given position
checkBack :: WordSearchGrid -> String -> Posn -> Bool
checkBack grid word (col,row) = checkForward grid (reverse word) (col,row)

--Insert the word backward (only called after checkBack has returned true to ensure the word fits)
insertBack :: WordSearchGrid -> String -> Posn -> WordSearchGrid
insertBack grid word (col,row) = insertForward grid (reverse word) (col,row)

--Checks if a word can fit down at a given position
checkDown :: WordSearchGrid -> String -> Posn -> Bool
checkDown grid [] (col,row) = True
checkDown grid (x:xs) (col,row) | row < length grid && (grid !! row !! col) == '-' = checkDown grid xs (col,row+1)
                                | otherwise = False

--Insert the word down (only called after checkDown has returned true to ensure the word fits)
insertDown :: WordSearchGrid -> String -> Posn -> WordSearchGrid
insertDown grid word (col,row) = take row grid ++ zipWith(curry (\ c -> take col (grid !! snd c) ++ fst c : drop (col + 1) (grid !! snd c))) word [row .. ] ++ drop (length word + row) grid

--Checks if a word can fit up at a given position
checkUp :: WordSearchGrid -> String -> Posn -> Bool
checkUp grid word (col,row) = checkDown grid (reverse word) (col,row) 

--Insert the word up(only called after checkUp has returned true to ensure the word fits)
insertUp :: WordSearchGrid -> String -> Posn -> WordSearchGrid
insertUp grid word (col,row) = insertDown grid (reverse word) (col,row)

--Checks if a word can fit diagonally forward down at a given position
checkForwardDown :: WordSearchGrid -> String -> Posn -> Bool
checkForwardDown grid [] (col,row) = True
checkForwardDown grid (x:xs) (col,row) | col < length (grid !! 1) && row < length grid && (grid !! row !! col) == '-' = checkForwardDown grid xs (col+1,row+1)
                                       | otherwise = False

--Insert the word diagonally forward down(only called after checkForwardDown has returned true to ensure the word fits)
insertForwardDown :: WordSearchGrid -> String -> Posn -> WordSearchGrid
insertForwardDown grid word (col,row) = take row grid ++ zipWith (curry (\ c-> take (col + snd c - row) (grid !! snd c) ++ fst c : drop (col + snd c + 1 - row) (grid !! snd c))) word [row .. ] ++ drop (length word + row) grid

--Checks if a word can fit diagonally backwards up at a given position by checking if it will fit in the same space as forward down
checkBackUp :: WordSearchGrid -> String -> Posn -> Bool
checkBackUp grid word (col,row) = checkForwardDown grid word (col,row)

--Insert the word diagonally backwards up(only called after checkBackUp has returned true to ensure the word fits)
insertBackUp :: WordSearchGrid -> String -> Posn -> WordSearchGrid
insertBackUp grid word (col,row) = insertForwardDown grid (reverse word) (col,row)

--Checks if a word can fit diagonally forward up at a given position by checking forward down on the reverse of the grid
checkForwardUp :: WordSearchGrid -> String -> Posn -> Bool
checkForwardUp grid word (col,row) = checkForwardDown (reverse grid) word (col,row)

--Insert the word diagonally forwards up(only called after checkForwardUp has returned true to ensure the word fits)
insertForwardUp :: WordSearchGrid -> String -> Posn -> WordSearchGrid
insertForwardUp grid word (col,row) = reverse (insertForwardDown (reverse grid) word (col,row))

--Checks if a word can fit diagonally back down at a given position by checkiing forward up on the reverse of the word
checkBackDown :: WordSearchGrid -> String -> Posn -> Bool
checkBackDown grid word (col,row) = checkForwardUp grid (reverse word) (col,row)

--Insert the word diagonally backwards down(only called after checkBackDown has returned true to ensure the word fits)
insertBackDown :: WordSearchGrid -> String -> Posn -> WordSearchGrid
insertBackDown grid word (col,row) = insertForwardUp grid (reverse word) (col,row)

--Finds the longest of the geiven words and returns its length
--Used to check that the given density makes a big enough grid for the longest word to fit in
longestStringLen :: [String] -> Int
longestStringLen list = fst $ maximum $ [(length xs, xs) | xs <- list]

--returns true if the newly inserted character forms a word 
isValidCharacter :: [String] -> WordSearchGrid -> Posn -> Bool
isValidCharacter [] grid pos = True
isValidCharacter (x:xs) grid pos | isNothing (snd (searchForWordAtPosition x grid pos)) && isNothing (snd (searchForWordAtPosition (reverse x) grid pos)) = isValidCharacter xs grid pos
                                 | otherwise = False

replaceTempChars :: [Char] -> [String] -> [(Int, Int)] -> WordSearchGrid -> IO WordSearchGrid
replaceTempChars chars words [] grid = return grid
replaceTempChars chars words (x:xs) grid = do randomChar <- chooseRandomFromList chars
                                              newGrid <- setCharAtPos x randomChar grid
                                              if isValidCharacter words newGrid x then replaceTempChars chars words xs newGrid
                                                else replaceTempChars chars words (x:xs) newGrid

setCharInList :: Int -> a -> [a] -> [a]
setCharInList n x xs = take n xs ++ (x : drop (n+1) xs)

setCharAtPos :: (Int, Int) -> Char -> WordSearchGrid -> IO WordSearchGrid
setCharAtPos (m,n) x xs = return (setCharInList n (setCharInList m x $ xs!!n) xs)

findTempChars :: WordSearchGrid -> [(Int, Int)]
findTempChars grid = [ (x,y) | (y,line) <- zip [0..] grid, x <- elemIndices '-' line ]

--Overarching function to create the word search and catch any issues with the given density for the given words
createWordSearch :: [ String ] -> Double -> IO WordSearchGrid
createWordSearch [] _ = error "No Strings given"
createWordSearch _ x             | x < 0 || x > 1 = error "Given density is not between 0 and 1"
createWordSearch strings density | calculateGridSize strings density < longestStringLen strings = error "Given density cannot produce a large enough grid for strings"
                                 | otherwise = do wordGrid <- insertWords (makeEmptyGrid (calculateGridSize strings density)) strings
                                                  replaceTempChars (getCharacterList strings) strings (findTempChars wordGrid) wordGrid

--- Convenience functions supplied for testing purposes
createAndSolve :: [ String ] -> Double -> IO [ (String, Maybe Placement) ]
createAndSolve words maxDensity = do g <- createWordSearch words maxDensity
                                     let soln = solveWordSearch words g
                                     printGrid g
                                     return soln

printGrid :: WordSearchGrid -> IO ()
printGrid [] = return ()
printGrid (w:ws) = do putStrLn w
                      printGrid ws

-- Challenge 3 --

prettyPrint :: LamMacroExpr -> String
prettyPrint (LamDef [] lam) = prettyPrintLam lam
prettyPrint (LamDef xs lam) = prettyPrintDefs xs ++ prettyPrintLam (substituteLam lam xs)

-- Substitute definitions
substituteLam :: LamExpr -> [(String,LamExpr)] -> LamExpr
substituteLam e@(LamVar _) defs         | e `elem` map snd defs = LamMacro (getSymbol e defs)
                                        | otherwise = e
substituteLam e@(LamAbs x lam) defs     | e `elem` map snd defs = LamMacro (getSymbol e defs)
                                        | otherwise = LamAbs x (substituteLam lam defs)
substituteLam e@(LamApp lam1 lam2) defs | e `elem` map snd defs = LamMacro (getSymbol e defs)
                                        | otherwise = LamApp (substituteLam lam1 defs) (substituteLam lam2 defs)
substituteLam e@(LamMacro _) defs = e

getSymbol :: LamExpr -> [(String, LamExpr)] -> String
getSymbol e [] = error "Failed to find macro symbol, ensure list of definitions contains both the macro symbol and corresponding lamda expression for each element"
getSymbol e ((macro,lam):xs) | e == lam = macro
                             | otherwise = getSymbol e xs

-- Print Output
prettyPrintDefs :: [(String,LamExpr)] -> String
prettyPrintDefs [x]    = "def " ++ fst x ++ " = " ++ prettyPrintLam (snd x) ++ " in "
prettyPrintDefs (x:xs) = "def " ++ fst x ++ " = " ++ prettyPrintLam (snd x) ++ " in " ++ prettyPrintDefs xs

-- Print lambda expression
prettyPrintLam :: LamExpr -> String
prettyPrintLam (LamVar x)         = "x" ++ show x 
prettyPrintLam (LamAbs x lam)     = "\\x" ++ show x ++ " -> " ++ prettyPrintLam lam
prettyPrintLam (LamApp lam1 lam2) = prettyPrintApp lam1 lam2
prettyPrintLam (LamMacro x)       = x

-- Print lambda exression with two expressions
prettyPrintLam' :: LamExpr -> LamExpr -> String
prettyPrintLam' exp1 exp2 = prettyPrintLam exp1 ++ "(" ++ prettyPrintLam exp2 ++ ")"

-- Print lambda application 
prettyPrintApp :: LamExpr -> LamExpr -> String
prettyPrintApp (LamApp a1 a2@LamAbs{}) exp2 = "(" ++ prettyPrintLam' a1 a2 ++ ") " ++ prettyPrintLam exp2
prettyPrintApp exp1@LamAbs{} exp2           = "(" ++ prettyPrintLam exp1 ++ ") " ++ prettyPrintLam exp2 
prettyPrintApp exp1 exp2@LamApp{}           = prettyPrintLam exp1 ++ " (" ++ prettyPrintLam exp2 ++ ")"
prettyPrintApp exp1 exp2                    = prettyPrintLam exp1 ++ " " ++ prettyPrintLam exp2

-- examples in the instructions
--ex3'1 = (LamDef [] (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1)))) --> "(\x1 -> x1) \x1 -> x1" 
--ex3'2 = (LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1))))) --> "\x1 -> x1 \x1 -> x1"
--ex3'3 = (LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamVar 2) (LamMacro "F")))) --> "def F = \x1 -> x1 in \x2 -> x2 F"
--ex3'4 = (LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2)))) --> def F = \x1-> x1 in \x2 -> F x2"

-- Challenge 4 --

--data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read)
--data LamExpr = LamMacro String | LamApp LamExpr LamExpr  |
--               LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read)

parseLamMacro :: String -> Maybe LamMacroExpr
parseLamMacro string = case parse lamMacroExpr string of
                          [(a,"")] -> Just a
                          _        -> Nothing

lamMacroExpr :: Parser LamMacroExpr
lamMacroExpr = lamMacroExpr'' <|> lamMacroExpr'

lamMacroExpr' :: Parser LamMacroExpr
lamMacroExpr' = do LamDef [] <$> lamExpr

lamMacroExpr'' :: Parser LamMacroExpr
lamMacroExpr'' = do macros <- macroList
                    symbol "in"
                    LamDef macros <$> lamExpr'

macroList :: Parser [(String, LamExpr)]
macroList = many macro

macro :: Parser (String, LamExpr)
macro = do symbol "def"
           name <- upperString
           symbol "="
           e <- lamExpr
           return (name, e)

lamExpr :: Parser LamExpr
lamExpr = bracketedExprExpr <|> bracketedExprExpr' <|> bracketedExpr <|> varExpr <|> macroExpr <|> lamAbs <|> lamVar <|> lamMacro 

--Ensures the lamExpr after "in" is not just a lamMacro
lamExpr' :: Parser LamExpr
lamExpr' = bracketedExprExpr <|> bracketedExprExpr' <|> bracketedExpr <|>  varExpr <|> macroExpr <|> lamAbs <|> lamVar

terminalExpr :: Parser LamExpr
terminalExpr = bracketedExpr <|> lamVar <|> lamMacro

--Handles multiple expressions 
expr' :: LamExpr -> Parser LamExpr
expr' e = exprExpr' e <|> expr'' e

--Takes expression, matches terminal expression
expr'' :: LamExpr -> Parser LamExpr
expr'' e = do LamApp e <$> terminalExpr

--Matches a terminal expression followed by an expression
exprExpr' :: LamExpr -> Parser LamExpr
exprExpr' e = do e1 <- terminalExpr
                 symbol ""
                 expr' (LamApp e e1)

bracketedExpr :: Parser LamExpr
bracketedExpr = do symbol "("
                   e <- lamExpr
                   symbol ")"
                   return e

bracketedExprExpr :: Parser LamExpr
bracketedExprExpr = do symbol "("
                       e1 <- lamExpr
                       symbol ""
                       e2 <- lamExpr
                       symbol ")"
                       return (LamApp e1 e2)

--Matches bracketed expression followed by another expression 
bracketedExprExpr' :: Parser LamExpr
bracketedExprExpr' = do e <- bracketedExpr
                        symbol ""
                        expr' e

--Matches a variable followed by another expression
varExpr :: Parser LamExpr
varExpr = do e <- lamVar
             symbol ""
             expr' e

macroExpr :: Parser LamExpr
macroExpr = do e <- lamMacro
               symbol ""
               expr' e

lamAbs :: Parser LamExpr
lamAbs = do symbol "\\"
            x <- idInt
            symbol "->"
            LamAbs x <$> lamExpr
            
idInt :: Parser Int
idInt = do symbol "x"
           natural

lamVar :: Parser LamExpr
lamVar = do symbol "x"
            LamVar <$> natural

lamMacro :: Parser LamExpr
lamMacro = do LamMacro <$> upperString

-- Helpers
upperString :: Parser String
upperString = do some upperChar

upperChar :: Parser Char
upperChar = sat isUpper


-- Challenge 5

cpsTransform :: LamMacroExpr -> LamMacroExpr
cpsTransform _ = LamDef [] (LamVar 0)

-- Examples in the instructions
exId =  LamAbs 1 (LamVar 1)
ex5'1 = (LamApp (LamVar 1) (LamVar 2))
ex5'2 = (LamDef [ ("F", exId) ] (LamVar 2) )
ex5'3 = (LamDef [ ("F", exId) ] (LamMacro "F") )
ex5'4 = (LamDef [ ("F", exId) ] (LamApp (LamMacro "F") (LamMacro "F")))


-- Challenge 6

innerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
innerRedn1 _ = Nothing

outerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
outerRedn1 _ = Nothing

compareInnerOuter :: LamMacroExpr -> Int -> (Maybe Int,Maybe Int,Maybe Int,Maybe Int)
compareInnerOuter _ _ = (Nothing,Nothing,Nothing,Nothing) 

-- Examples in the instructions

-- (\x1 -> x1 x2)
ex6'1 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamVar 2)))

--  def F = \x1 -> x1 in F  
ex6'2 = LamDef [ ("F",exId) ] (LamMacro "F")

--  (\x1 -> x1) (\x2 -> x2)   
ex6'3 = LamDef [] ( LamApp exId (LamAbs 2 (LamVar 2)))

--  (\x1 -> x1 x1)(\x1 -> x1 x1)  
wExp = (LamAbs 1 (LamApp (LamVar 1) (LamVar 1)))
ex6'4 = LamDef [] (LamApp wExp wExp)

--  def ID = \x1 -> x1 in def FST = (\x1 -> λx2 -> x1) in FST x3 (ID x4) 
ex6'5 = LamDef [ ("ID",exId) , ("FST",LamAbs 1 (LamAbs 2 (LamVar 1))) ] ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (LamMacro "ID") (LamVar 4)))

--  def FST = (\x1 -> λx2 -> x1) in FST x3 ((\x1 ->x1) x4))   
ex6'6 = LamDef [ ("FST", LamAbs 1 (LamAbs 2 (LamVar 1)) ) ]  ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (exId) (LamVar 4)))

-- def ID = \x1 -> x1 in def SND = (\x1 -> λx2 -> x2) in SND ((\x1 -> x1) (\x2 -> x2)) ID
ex6'7 = LamDef [ ("ID",exId) , ("SND",LamAbs 1 (LamAbs 2 (LamVar 2))) ]  (LamApp (LamApp (LamMacro "SND") (LamApp wExp wExp) ) (LamMacro "ID") ) 
