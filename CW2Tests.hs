import Challenges
import Parsing
import Data.Maybe

tests :: [(String, [(String,Bool)])]
tests = 
    [
    ("Challenge 1",
        [
        ("Test 1: Looking for HASKELL, STRING ,STACK, MAIN, METHOD in a grid containing the words",
        checkFoundWords (solveWordSearch c1Words1 c1Grid1) c1Solution1),

        ("Test 2: Looking for BANANA, ORANGE, MELON, RASPBERRY, APPLE, PLUM, GRAPE in a grid containing the words",
        checkFoundWords (solveWordSearch c1Words2 c1Grid2) c1Solution2),

        ("Test 3: Check that given no words to find in a grid, the empty list is returned",
        checkFoundWords (solveWordSearch [] c1Grid1) [])
        ]
    ),

    ("Challenge 2 - Tested seperately due to type restrictions",
        []
    ),

    ("Challenge 3",
        [
        ("Test 1: Check 'LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1))))' pretty prints to '(\\x1 -> x1) \\x1 -> x1' ",
        checkPrettyPrint (prettyPrint (LamDef [] (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1)))) ) "(\\x1 -> x1) \\x1 -> x1"  ),
        
        ("Test 2: Check 'LamDef [] (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1)))' pretty prints to '\\x1 -> x1 \\x1 -> x1' ",
        checkPrettyPrint (prettyPrint (LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1))))) ) "\\x1 -> x1 \\x1 -> x1"),

        ("Test 3: Check 'LamDef [ (\"F\", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamVar 2) (LamMacro \"F\")))' pretty prints to 'def F = \\x1 -> x1 in \\x2 -> x2 F' ",
        checkPrettyPrint (prettyPrint (LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamVar 2) (LamMacro "F")))) ) "def F = \\x1 -> x1 in \\x2 -> x2 F"),

        ("Test 4: Check 'LamDef [ (\"F\", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2)))' pretty prints to 'def F = \\x1 -> x1 in \\x2 -> F x2' ",
        checkPrettyPrint (prettyPrint (LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2)))) ) "def F = \\x1 -> x1 in \\x2 -> F x2")
        ]
    ),

    ("Challenge 4 (\\'s suitably escaped on input)",
        [
        ("Test 1: Check parsing the expression 'x1 (x2 x3)' returns 'Just (LamDef [] (LamApp (LamVar 1) (LamApp(LamVar 2) (LamVar 3))))'",
        checkLamMacroParser (parseLamMacro "x1 (x2 x3)") (Just (LamDef [] (LamApp (LamVar 1) (LamApp(LamVar 2) (LamVar 3))))) ),
        
        ("Test 2: Check parsing the expression 'x1 x2 F' returns 'Just (LamDef [] (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamMacro \"F\"))' ",
        checkLamMacroParser (parseLamMacro "x1 x2 F") (Just (LamDef [] (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamMacro "F")))) ),

        ("Test 3: Check parsing the expression 'def F = \\x1 -> x1 in \\x2 -> x2 F' returns 'Just (LamDef [ (\"F\", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamVar 2) (LamMacro \"F\"))))' ",
        checkLamMacroParser (parseLamMacro "def F = \\x1 -> x1 in \\x2 -> x2 F") (Just (LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamVar 2) (LamMacro "F"))))) ),

        ("Test 4: Check parsing the expression 'def F = \\x1 -> x1 (def G = \\x1 -> x1 in x1) in \\x2 -> x2' returns 'Nothing' ",
        checkLamMacroParser (parseLamMacro "def F = \\x1 -> x1 (def G = \\x1 -> x1 in x1) in \\x2 -> x2") Nothing ),

        ("Test 5: Check parsing the expression 'def F = \\x1 -> x1 in def F = \\x2 -> x2 x1 in x1' returns 'Nothing' ",
        checkLamMacroParser (parseLamMacro "def F = \\x1 -> x1 in def F = \\x2 -> x2 x1 in x1") Nothing ),

        ("Test 6: Check parsing the expression 'def F = x1 in F' returns 'Nothing' ",
        checkLamMacroParser (parseLamMacro "def F = x1 in F") Nothing )
        ]
    ),

        ("Challenge 5",
        [
        ("Test 1: Check converting '(LamApp (LamVar 1) (LamVar 2))' returns a correct CPS expression",
        cpsTransformChecker (cpsTransform (LamDef [] (LamApp (LamVar 1) (LamVar 2)))) (LamDef [] (LamApp (LamVar 1) (LamVar 2))) )
        ]
    )
    ]

chal2Tests :: [(String, [String], Double)]
chal2Tests = 
    [
        ("Test 1: Generate and solve a grid containing the words HASKELL, STRING, STACK, JUST, MAIN, METHOD and ensure it contains each of them exactly once",
        ["HASKELL", "STRING", "STACK", "JUST", "MAIN", "METHOD"],
        0.3),

        ("Test 2: Generate and solve a very large grid containing the words ALDER, COTTONWOOD, PINE, APPLE, CYPRESS, POPLAR, ASH and ensure it contains each of them exactly once",
        ["ALDER", "COTTONWOOD", "PINE", "APPLE","CYPRESS","POPLAR","ASH"],
        0.01)
    ]

main :: IO ()
main = do putStrLn ""
          testEachChallenge tests
          putStrLn "-- Challenge 2 --"
          testChallenge2 chal2Tests


testEachChallenge :: [(String, [(String,Bool)])] -> IO ()
testEachChallenge [] = putStr "" 
testEachChallenge ((c,tests):cs) = do putStrLn ("-- " ++ c ++ " -- " ++ getChallengeScore tests ++ " --")
                                      testChallenge tests
                                      putStrLn ""
                                      testEachChallenge cs

testChallenge :: [(String, Bool)] -> IO ()
testChallenge [] = putStr  ""
testChallenge ((message, False) : ts) = do putStr "Test Failed: "
                                           putStrLn message
                                           testChallenge ts
testChallenge ((message, True) : ts)  = do  putStr "Test Passed: "
                                            putStrLn message
                                            testChallenge ts

-- Seperate tester for challenge 2 due to it returning an IO type
testChallenge2 :: [(String, [String], Double)] -> IO ()
testChallenge2 [] = putStr ""
testChallenge2 (( message, words, density) : ts) = do result <- createAndSolveCheck words density
                                                      putStr result
                                                      putStrLn message
                                                      testChallenge2 ts
                                                      

getChallengeScore :: [(String, Bool)] -> String
getChallengeScore cs = "Passed " ++ show (length [(string, bool) | (string, bool) <- cs , bool]) ++ " out of " ++ show (length cs) ++ " tests"

-- Challenge 1 output checker
checkFoundWords :: [ (String,Maybe Placement) ] -> [ (String,Maybe Placement) ] -> Bool
checkFoundWords x y = x == y

-- Challenge 2 output checker 
checkGeneration :: [ (String,Maybe Placement) ] -> Bool
checkGeneration [] = True
checkGeneration (x:xs) | isNothing (snd x) = False
                       | otherwise = checkGeneration xs

createAndSolveCheck :: [ String ] -> Double -> IO String
createAndSolveCheck words maxDensity = do g <- createWordSearch words maxDensity
                                          let soln = solveWordSearch words g
                                          let check = checkGeneration soln
                                          let b = (length soln == length words) && check in
                                              if b then return "Test Passed: "
                                                else return "Test failed: "

-- Challenge 3 output checker
checkPrettyPrint :: String -> String -> Bool
checkPrettyPrint x y = x == y

-- Challege 4 output checker
checkLamMacroParser :: Maybe LamMacroExpr -> Maybe LamMacroExpr -> Bool 
checkLamMacroParser x y = x == y

-- Challenge 5 checker
cpsTransformChecker :: LamMacroExpr -> LamMacroExpr -> Bool
cpsTransformChecker x y = x == y
                                     

-- Challenge 1 test varialbes
c1Grid1 = [ "HAGNIRTSH" , "SACAGETAK", "GCSTACKEL","MGHKMILKI","EKNLETGCN","TNIRTLETE","IRAAHCLSR","MAMROSAGD","GIZKDDNRG" ] 
c1Words1 = [ "HASKELL","STRING","STACK","MAIN","METHOD"]
c1Solution1 = [("HASKELL",Just((0,0),DownForward)),("STRING",Just((7,0),Back)),("STACK",Just((2,2),Forward)),("MAIN",Just((2,7),Up)),("METHOD",Just((4,3),Down))]

c1Grid2 = ["ROBREUMBR","AURPEPSAN","UNLALMSEE","YGAUNPYYP","NLMNBGENA","NBLEALEOR","ALRYPBBLG","NREPBEBEP","YGAYAROMR"]
c1Words2 = [ "BANANA", "ORANGE", "MELON", "RASPBERRY","APPLE","PLUM","GRAPE" ]
c1Solution2 = [("BANANA",Just ((5,6),UpBack)),("ORANGE",Just ((1,0),DownForward)),("MELON",Just ((7,8),Up)),("RASPBERRY",Just ((8,0),DownBack)),("APPLE",Just ((2,8),UpForward)),("PLUM",Just ((5,1),DownBack)),("GRAPE",Just ((8,6),Up))]

-- Challenge 2 variables
c2Words1 = [ "HELLO" , "WORLD" , "HASKELL" , "PUZZLE"]