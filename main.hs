-- TESTABLE CASES:
-- SIMPLE PRINT STATEMENTS WITH STRINGS
-- DECLARING VARIABLES (int, float, bool, string)
-- SIMPLE TWO-OPERAND ARITHMETIC (+, -, *, /, //, %) (PARTIAL)
-- started first steps of conditional statement checking
-- (will spend significantly more time refining and adding before finished)
import System.IO                            -- needed for OpenFile
import Data.List (isPrefixOf, isInfixOf)    -- needed to identify leading keywords ("print") and assignment operator
import Data.List (delete, stripPrefix)      -- delete removes first occurrence of substring
import Data.List (dropWhileEnd)             -- needed to trim trailing white space
import Data.List.Split (splitOn)            -- needed for string extraction (concat)
import Data.Char (isSpace)                  -- needed to trim leading and trailing white space
import Data.Char (isPunctuation, isSymbol, isDigit)  -- needed to check for special characters
import Data.Typeable                        -- needed to determine the type of a variable (using PyVar)
import qualified Data.Text as T             -- (breakOn, append, drop, length)
import Control.Monad(unless)                -- needed for function loop
import Text.Read (readMaybe)

-- write data constructors to define Python variable types
data PyVar = PyInt Integer | PyFloat Float | PyBool Bool | PyStr String | Unknown deriving (Eq, Show)

-- write data constructors to define Python operators (arithmetic and comparison)
data PyArith = PyAdd | PySub | PyMul | PyDiv | PyFloorDiv | PyMod deriving Show
data PyCompare = PyEq | PyNEq | PyLT | PyLTE | PyGT | PyGTE deriving Show

-- write data constructors to define two-operand Python arithmetic expressions
data PyExpr = VarDecl String PyVar
            | ArithAssign String String PyArith PyVar  -- dest = lhs op rhs
            deriving Show

-- write data constructors to define Python conditional keywords (if, elif, else)
data PyCondWord = PyIf | PyElif deriving Show
data PyCondFinal = PyElse deriving Show

-- write data constructors to define simple Python conditional statements
data PyCondExpr = PyCondWord PyVar PyCompare PyVar      -- i.e. if x == 5, elif y >= x
                -- | PyCondWord PyVar PyCompare PyVar
                | PyCondFinal deriving Show     -- else

-- simple function that removes leading whitespace from a line of code
-- key benefit: helps eliminate indentation when looking for prefix keywords
noLeadingSpace :: String -> String
noLeadingSpace = dropWhile isSpace

-- simple function that removes trailing whitespace from a line of code
-- key benefit: removes extra trailing space in string literals in MIPS
noTrailingSpace :: String -> String
noTrailingSpace = dropWhileEnd isSpace

-- simple function that determines whether a line of code is empty
isEmpty :: String -> Bool
isEmpty str = all isSpace str

-- function that removes code that is not a string literal or a variable
-- Purpose: to display a string literal or variable in MIPS without the surrounding Python syntax
extractString :: String -> IO String
extractString trimmed = do
    let extracted = maybe trimmed id (stripPrefix "print(" trimmed)
    
    -- ADD CODE TO REMOVE COMMENTS (CALL SKIP_COMMENTS FUNCTION HERE - NOT CODED YET)
    
    -- remove trailing space and final parenthesis
    let extracted1 = noTrailingSpace extracted
    return (init extracted1)

-- extract variable name and value
extractVariable :: String -> Maybe (String, PyVar)
extractVariable trimmed = case words trimmed of
    (var : eq : rest) | eq == "=" && isValidName var -> 
        Just (var, parseValue (unwords rest))
    _ -> Nothing

-- extract both operands and operator from an expression
extractOperands :: String -> Maybe (String, String, String, PyArith, PyVar)
extractOperands trimmed = 
  let w = words trimmed in
  trace ("Input line: '" ++ trimmed ++ "' -> words: " ++ show w) $
  case w of
    (dest:eq:lhs:opStr:rhs) 
      | eq == "=" -> 
          trace ("opStr='" ++ opStr ++ "'") $
          case parseArith opStr of
            Just op -> 
              let rhsVal = parseValue (unwords rhs) in
              if rhsVal /= Unknown 
              then trace ("SUCCESS: " ++ dest ++ " = " ++ lhs ++ " " ++ show op ++ " " ++ show rhsVal) $
                   Just (dest, lhs, "", op, rhsVal)
              else Nothing
            Nothing -> Nothing
    _ -> Nothing
    
-- parse value string to PyVar (int, float, bool, string types)
parseValue :: String -> PyVar
parseValue val =
  let raw = val
      wordsOnly = words val
      trimmed = if null wordsOnly then "" else head wordsOnly  -- take first word only
  
  -- check if boolean
  in if trimmed == "True" then PyBool True
  else if trimmed == "False" then PyBool False
  
  -- check if integer
  else case readMaybe trimmed :: Maybe Integer of
    Just n -> PyInt n
    -- check if float
    Nothing -> case readMaybe trimmed :: Maybe Float of
      Just f -> PyFloat f
      Nothing -> -- check if string
        -- strip one pair of quotes from ends
        if length trimmed >= 2 
           && (head trimmed == '"' && last trimmed == '"' 
               || head trimmed == '\'' && last trimmed == '\'')
        then PyStr (take (length trimmed - 2) (drop 1 trimmed))
        else Unknown

-- parse Python arithmetic operators (used for computation)
parseArith :: String -> Maybe PyArith
parseArith op = 
  case op of
    "+"  -> Just PyAdd
    "-"  -> Just PySub
    "*"  -> Just PyMul
    "/"  -> Just PyDiv
    "//" -> Just PyFloorDiv
    "%"  -> Just PyMod
    _    -> Nothing

-- parse Python comparison operators (used in conditional statements)
parseCompare :: String -> Maybe PyCompare
parseCompare op = 
  case op of
    "==" -> Just PyEq
    "!=" -> Just PyNEq
    "<"  -> Just PyLT
    "<=" -> Just PyLTE
    ">"  -> Just PyGT
    ">=" -> Just PyGTE
    _    -> Nothing

-- function that determines whether variable declaration is valid
isVarInit :: String -> Bool
isVarInit trimmed = case words trimmed of
    (var : eq : _) | eq == "="  -> isValidName var
    _                           -> False

-- function that determines whether a variable name is valid
isValidName :: String -> Bool
isValidName [] = False
isValidName (char:trimmed)
    | not (isValidLead char) = False
    | otherwise = all isValidFollow trimmed
    where
        isValidLead char = char `elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
        isValidFollow char = char `elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['_'] ++ ['0'..'9']

-- function that checks for special characters in variable declarations at a certain index
isSpecial :: String -> Int -> Bool
isSpecial str index =
    let char = str !! index in
        if (char == '\"')    -- exclude double quotes, since they are needed for string literals
            then False
        else (isPunctuation char || isSymbol char)

-- function that translates a print statement into assembly code and writes into MIPS
translatePrint :: String -> FilePath -> FilePath -> Int -> IO()
translatePrint str mData mCode strCount = do
    -- write .data translation
    appendFile mData ("str" ++ show strCount ++ ": .asciiz \"" ++ str ++ "\"\n")
    
    -- write .text translation
    appendFile mCode ("\tla $a0, str" ++ show strCount ++ "\n" ++ "\tli $v0, 4\n")
    appendFile mCode ("\tsyscall\n\n")

    -- display message to terminal
    putStrLn "translatePrint successful"

-- function that translates a variable declaration into MIPS
translateVariable :: String -> PyVar -> FilePath -> FilePath -> Int -> Int -> IO()
translateVariable name value mData mCode treg freg = do
    -- write .data and .text translation based on variable type
    case value of
        -- check integer type
        PyInt int -> do
            appendFile mData (name ++ ":\t.word\t" ++ show int ++ "\n")
            appendFile mCode ("\tli $t" ++ show treg ++ ", " ++ name ++ "\n")
        
        -- check float type
        PyFloat float -> do
            appendFile mData (name ++ ":\t.float\t" ++ show float ++ "\n")
            appendFile mCode ("\tl.s $f" ++ show freg ++ ", " ++ name ++ "\n")
        
        -- check boolean type (WILL COMBINE WITH INT FOR CONCISENESS IF POSSIBLE)
        PyBool bool -> do
            let intVal = if bool then 1 else 0
            appendFile mData (name ++ ":\t.word\t" ++ show intVal ++ "\n")
            appendFile mCode ("\tli $t" ++ show treg ++ ", " ++ name ++ "\n")
        
        -- check string type (nothing written in .text section since str is not used)
        PyStr str -> appendFile mData (name ++ ":\t.asciiz\t\"" ++ str ++ "\"\n")
        
        -- check unknown type
        Unknown -> putStrLn "Unknown type"

    -- display message to terminal
    putStrLn "translateVariable successful"
    
    -- END OF TRANSLATE_VARIABLE

-- function that translates a two-operand arithmetic expression into MIPS (SHELL)
translateExpression :: String -> String -> PyArith -> PyVar -> FilePath -> FilePath -> Int -> Int -> IO()
translateExpression dest lhs op rhs mData mCode treg freg = do
    putStrLn "translateExpression successful"

-- MAIN FUNCTION
main :: IO()
main = do
    -- define file names as variables to help readability
    let python = "test.py"
    let mipsData = "data.txt"
    let mipsCode = "code.txt"
    let mipsFinal = "assembly.s"
    
    -- define integers used for MIPS variable names
    let strCount = 0
    let tregCount = 0
    let fregCount = 0
    
    -- write default code to MIPS data and code sections, separate into two .txt files until done
    writeFile mipsData "\t.data\n"
    writeFile mipsCode ("\t.text\n" ++ "\t.globl main\n" ++ "main:\n")
    
    -- open Python file handle for line-by-line reading
    withFile python ReadMode $ \h -> do
        loop h mipsData mipsCode strCount tregCount fregCount  -- start the while loop
    putStrLn "\nWriting to data.txt and code.txt successful"
    
    -- write end program logic into code.txt
    appendFile mipsCode "\tjr $ra"
    
    -- merge the two .txt files into assembly.s (FINAL STEP OF THE WHOLE PROGRAM)
    dataContents <- readFile mipsData
    codeContents <- readFile mipsCode
    writeFile mipsFinal (dataContents ++ "\n" ++ codeContents)
    putStrLn "Writing to assembly.s successful"

-- WHILE NOT EOF LOOP (continues until end of test.py)
loop :: Handle -> String -> String -> Int -> Int -> Int -> IO ()
loop h mipsData mipsCode strCount tregCount fregCount = do
    eof <- hIsEOF h
    unless eof $ do
        -- read next line and trim leading whitespace
        pyLine <- hGetLine h
        let trimmedLine = noLeadingSpace pyLine
        
        -- IF CHARACTER AT trimmedLine INDEX 0 IS '#', THEN IGNORE COMPLETELY AND MOVE TO NEXT LINE
        -- REFACTOR SO THAT STRING LITERALS CAN ALSO IGNORE COMMENTS IN TRANSLATE_PRINT
        -- SAMPLE FUNCTION TEMPLATE:
        -- skipComments :: String -> String or IO String
        -- skipComments trimmedLine = do
        -- <<insert code seen below>>
        case trimmedLine of
            ('#':_) -> loop h mipsData mipsCode strCount tregCount fregCount  -- comment line, skip
            _       -> do  -- non-comment, process normally
                if (isEmpty trimmedLine) then
                    putStrLn $ "Empty line skipped"
                else do putStr $ ""
            
        -- trim whitespace and find whether "print" is a prefix of the full string
        let isPrint = "print(" `isPrefixOf` trimmedLine && not (isSpecial trimmedLine 6)    -- trim whitespace and find whether "print" is a prefix of the full string
        let isVar = "=" `isInfixOf` trimmedLine && not (isPrint) -- CHECK IF INFIX CHECK IS REDUNDANT
        let isArith = if (not(isPrint) && not(isVar)) then True
                      else False -- BAND-AID SOLUTION, WILL MODIFY WHEN MORE STATEMENTS ARE TESTED
        let isFunc = "def " `isPrefixOf` trimmedLine
        let isFuncReturn = "return " `isPrefixOf` trimmedLine
        
        -- update count variables
        -- strCount used for variable names in MIPS (i.e. str1, str2, str3, etc.)
        -- *regCount variables used for register names in MIPS (i.e. $t0, $t1, $t2, $f0, $f1, $f2, etc.)
        let strCount' = if isPrint then (strCount + 1)
                        else strCount
        let tregCount' = if isVar then ((tregCount + 1) `rem` 10)
                        else tregCount
        let fregCount' = if isVar then ((fregCount + 1) `rem` 32)
                        else fregCount
        
        -- map a value to each statement type for cleaner evaluation
        let result = if isPrint then 0 
                     else if isVar then 1
                     else if isArith then 2
                     -- else if isCond then 3
                     -- else if isCase then 4
                     -- else if isLoop then 5
                     else if isFunc then 6
                     else if isFuncReturn then 7
                     else -1
        
        -- translate line into MIPS based on the type of statement
        case result of
            0 -> do
                -- translate print line
                strLiteral <- extractString trimmedLine
                translatePrint strLiteral mipsData mipsCode strCount'
            1 -> do
                -- translate simple variable declaration
                maybeVar <- pure $ extractVariable trimmedLine
                case maybeVar of
                    Just (varName, varValue) -> do
                        -- Use varName and varValue
                        putStrLn $ "Var: " ++ varName ++ " = " ++ show varValue
                        translateVariable varName varValue mipsData mipsCode tregCount' fregCount' -- varName and varValue now in scope for whole do block
                    Nothing -> return()
            2 -> do
                -- translate two-operand arithmetic operations
                maybeVar <- pure $ extractOperands trimmedLine
                case maybeVar of
                    Just (destVar, lhs, rhsVar, op, rhsVal) -> do
                        -- Use varName and varValue
                        putStrLn $ "Expression: " ++ destVar ++ " = " ++ show lhs ++ " " ++ show op ++ " " ++ show rhsVal
                        translateExpression destVar lhs op rhsVal mipsData mipsCode tregCount' fregCount'
                    Nothing -> return()
                putStrLn $ "Arithmetic operation (code abstracted)"
            6 -> do
                -- translate function header
                putStrLn $ "Function header (code abstracted)"
            7 -> do
                -- translate function return statement
                putStrLn $ "Function return (code abstracted)"
            -1 -> do
                if (not(isEmpty trimmedLine)) then
                    putStrLn $ "Encountered invalid/untested operation"
                else return()
            _ -> do
                putStrLn $ "Computation error"
        
        -- move to the next line using recursion
        loop h mipsData mipsCode strCount' tregCount' fregCount'
        
