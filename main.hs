-- TESTABLE CASES:
-- SIMPLE PRINT STATEMENTS WITH STRINGS
-- DECLARING VARIABLES (int, float, bool, string)
-- SIMPLE TWO-OPERAND ARITHMETIC (+, -, *, /, //, %) (PARTIALLY DONE)
-- started first steps of conditional statement checking
-- TESTABLE FRINGE CASES: Python file does not exist or is empty
-- (will spend significantly more time refining and adding before finished)
import System.IO                            -- needed for OpenFile
import Data.List (isPrefixOf, isInfixOf)    -- needed to identify leading keywords ("print") and assignment operator
import Data.List (delete, stripPrefix)      -- delete removes first occurrence of substring
import Data.List (dropWhileEnd)             -- needed to trim trailing white space
import Data.List.Split (splitOn)            -- needed for string extraction (concat)
import Data.Char (isSpace, toLower)         -- isSpace needed to trim leading and trailing white space, toLower needed for bool variables
import Data.Char (isPunctuation, isSymbol, isDigit)  -- needed to check for special characters
import Data.Typeable                        -- needed to determine the type of a variable (using PyVar)
import Debug.Trace (trace)                  -- used for debugging
import Data.Maybe (isJust)                  -- needed for conditional statement checking
import Data.List (isSuffixOf)               -- needed for conditional statement checking
import qualified Data.Text as T             -- (breakOn, append, drop, length)
import Control.Monad(unless)                -- needed for function loop
import Text.Read (readMaybe)                -- needed to parse a string into a data type
import Control.Exception (catch, IOException, throwIO)  -- needed to check empty file
import System.IO.Error (isDoesNotExistError)    -- neded to throw exception when Python file does not exist
import System.Exit (exitFailure)            -- needed to terminate the program when an exception is encountered

-- write data constructors to define Python variable types
data PyVar = PyInt Integer | PyFloat Float | PyBool Bool | PyStr String | Unknown deriving (Eq, Show)

-- write data constructors to define Python operators (arithmetic and comparison)
data PyArith = PyAdd | PySub | PyMul | PyDiv | PyFloorDiv | PyMod deriving Show
data PyCompare = PyEQ | PyNEQ | PyLT | PyLTE | PyGT | PyGTE deriving Show

-- write data constructors to define two-operand Python arithmetic expressions
data PyExpr = VarDecl String PyVar
            | ArithAssign String String PyArith PyVar  -- dest = lhs op rhs
            deriving Show

-- write data constructors to define Python conditional keywords (if, elif, else)
data PyCond = PyCond PyVar PyCompare PyVar deriving Show

-- write data constructors to define full conditional statements
data PyStmt
  = PyAssign String PyVar
  | PyPrint PyVar
  | PyIf PyCond [PyStmt]
  | PyElif PyCond [PyStmt]
  | PyElse [PyStmt]
  deriving Show

-- write data constructors to define simple Python conditional statements
{- data PyCondExpr = PyCondWord PyVar PyCompare PyVar      -- i.e. if x == 5, elif y >= x
                -- | PyCondWord PyVar PyCompare PyVar
                | PyCondFinal deriving Show     -- else -}
data PyIfChain = PyIfChain
  { ifCond     :: PyCond
  , ifBody     :: [PyStmt]
  , elifs      :: [(PyCond, [PyStmt])]
  , elseBody   :: Maybe [PyStmt]
  , indentLevel :: Int
  } deriving Show

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

-- function that throws an error if a file is found but empty
isEmptyFile :: FilePath -> IO()
isEmptyFile py = do
    contents <- readFile py
    if null contents then do    -- ALSO NEED TO CONSIDER SKIPPING COMMENTS
        putStrLn $ "Empty file exception: " ++ py ++ " was found but is empty."
        putStrLn $ "Writing .data and .text directives to assembly.s"
        exitFailure
    else putStrLn $ "File " ++ py ++ " is not empty."

-- function that throws an error if a file is not found
isFile :: FilePath -> IOException -> IO String
isFile py e
  | isDoesNotExistError e = do
      putStrLn $ "isDoesNotExistError: " ++ py ++ " not found, cannot translate code."
      exitFailure -- terminate the program imediately
  | otherwise = throwIO e

-- function that writes default code to .data and .text sections (data.txt and code.txt)
writeMipsDefault :: FilePath -> FilePath -> FilePath -> IO()
writeMipsDefault mData mCode mFinal = do
    -- write assembler directives to data.txt and code.txt
    writeFile mData "\t.data\n"
    writeFile mCode ("\t.text\n" ++ "\t.globl main\n" ++ "main:\n")
    mergeMips mData mCode mFinal

-- function that merges code.txt and data.txt into assembly.s (final MIPS file)
mergeMips :: FilePath -> FilePath -> FilePath -> IO()
mergeMips mData mCode mFinal = do
    dataContents <- readFile mData
    codeContents <- readFile mCode
    writeFile mFinal (dataContents ++ "\n" ++ codeContents)

-- function that removes code that is not a string literal or a variable
-- Purpose: to display a string literal or variable in MIPS without the surrounding Python syntax
extractString :: String -> IO String
extractString trimmed = do
    let extracted = maybe trimmed id (stripPrefix "print(" trimmed)
    
    -- ADD CODE TO REMOVE COMMENTS (CALL SKIP_COMMENTS FUNCTION HERE - NOT CODED YET)
    
    -- remove trailing space and final parenthesis
    let extracted1 = noTrailingSpace extracted
    return (init extracted1)

-- function that extracts variable name and value
extractVariable :: String -> Maybe (String, PyVar)
extractVariable trimmed = case words trimmed of
    (var : eq : rest) | eq == "=" && isValidName var -> 
        Just (var, parseValue (unwords rest))
    _ -> Nothing

-- function that extracts both operands and operator from an expression
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
              then trace ("SUCCESS: " ++ dest ++ " = " ++ lhs ++ " " ++ show op ++ " " ++ prettyPyVar rhsVal) $
                   Just (dest, lhs, "", op, rhsVal)
              else Nothing
            Nothing -> Nothing
    _ -> Nothing

-- function that parses an if statement line
extractIfLine :: String -> Maybe String
extractIfLine trimmed = 
  let w = words trimmed in
  case w of
    ("if":rest) -> Just $ unwords (takeWhile (/= ":") rest)
    _ -> Nothing

-- function that parses an elif statement line
extractElifLine :: String -> Maybe String
extractElifLine trimmed = 
  let w = words trimmed in
  case w of
    ("elif":rest) -> Just $ unwords (takeWhile (/= ":") rest)
    _ -> Nothing

-- function that parses an else statement line
extractElseLine :: String -> Maybe String
extractElseLine trimmed = 
  let w = words [if c == ':' then ' ' else c | c <- trimmed] in
  case w of
    ("else":_) -> Just "else"
    _ -> Nothing

-- helper function that renders variables and registers
loadVarToReg :: PyVar -> String -> FilePath -> IO ()
loadVarToReg var reg mCode = do
  case var of
    PyInt i   -> appendFile mCode $ "\tlw $" ++ reg ++ ", " ++ show i ++ "\n"
    PyFloat f -> appendFile mCode $ "\tl.s $f" ++ reg ++ ", " ++ show f ++ "\n"
    PyStr s   -> appendFile mCode $ "\tla $" ++ reg ++ ", " ++ s ++ "\n"
    PyBool b  -> appendFile mCode $ "\tli $" ++ reg ++ ", " ++ (if b then "1" else "0") ++ "\n"
    _         -> appendFile mCode $ "\tli $" ++ reg ++ ", 0\n"
    
-- function that parses value string to PyVar (int, float, bool, string types)
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

-- function that parses Python arithmetic operators (used for computation)
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

-- function that parses Python comparison operators (used in conditional statements)
parseCompare :: String -> Maybe PyCompare
parseCompare op = 
  case op of
    "==" -> Just PyEQ
    "!=" -> Just PyNEQ
    "<"  -> Just PyLT
    "<=" -> Just PyLTE
    ">"  -> Just PyGT
    ">=" -> Just PyGTE
    _    -> Nothing

-- function that renders a numerical operand in an arithmetic expression
renderOperand :: PyVar -> String
renderOperand (PyInt i)   = show i
renderOperand (PyFloat f) = show f
renderOperand (PyBool b)  = show b
renderOperand (PyStr s)   = s
renderOperand Unknown     = "0"
renderOperand v           = show v

-- function that extracts the raw value of a variable
prettyPyVar :: PyVar -> String
prettyPyVar (PyInt i)    = show i
prettyPyVar (PyFloat f)  = show f
prettyPyVar (PyBool b)   = map toLower (show b)
prettyPyVar (PyStr s)    = s
prettyPyVar Unknown      = "?"

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

-- function that translates a two-operand arithmetic expression into MIPS
translateExpression :: PyVar -> PyVar -> PyArith -> PyVar -> FilePath -> FilePath -> Int -> Int -> IO ()
translateExpression dest lhs op rhs mData mCode treg freg = do
  putStrLn $ "DEBUG lhs=" ++ show lhs ++ " | rhs=" ++ show rhs
  case op of
    PyAdd -> do
      case (lhs, rhs) of
        -- 1. Pure literals
        (PyInt l, PyInt r) -> do
          appendFile mCode ("\taddi $t" ++ show treg ++ ", $0, " ++ show l ++ "\n")
          let treg' = treg + 1
          appendFile mCode ("\taddi $t" ++ show treg' ++ ", $t" ++ show treg ++ ", " ++ show r ++ "\n")
          putStrLn "Add operation: PyInt + PyInt"
        
        (PyFloat l, PyFloat r) -> do
          appendFile mCode ("\tli.s $f" ++ show freg ++ ", " ++ show l ++ "\n")
          let freg' = freg + 1
          appendFile mCode ("\tadd.s $f" ++ show freg ++ ", $f" ++ show freg ++ ", $f" ++ show freg' ++ "\n")
          putStrLn "Add operation: PyFloat + PyFloat"
        
        (PyInt l, PyFloat r) -> do
          appendFile mCode ("\tli.s $f" ++ show freg ++ ", " ++ show l ++ "\n")
          let freg' = freg + 1
          appendFile mCode ("\tadd.s $f" ++ show freg ++ ", $f" ++ show freg ++ ", $f" ++ show freg' ++ "\n")
          putStrLn "Add operation: PyInt + PyFloat"
        
        (PyFloat l, PyInt r) -> do
          appendFile mCode ("\tli.s $f" ++ show freg ++ ", " ++ show l ++ "\n")
          let freg' = freg + 1
          appendFile mCode ("\tadd.s $f" ++ show freg ++ ", $f" ++ show freg ++ ", $f" ++ show freg' ++ "\\n")
          putStrLn "Add operation: PyFloat + PyInt"
        
        -- 5. Common: var + literal
        (_, PyInt r) -> do
          appendFile mCode ("\tlw $t" ++ show treg ++ ", " ++ prettyPyVar lhs ++ "\n")
          let treg' = treg + 1
          appendFile mCode ("\taddi $t" ++ show treg' ++ ", $t" ++ show treg ++ ", " ++ show r ++ "\n")
          putStrLn "Add operation: _ + PyInt"
        
        (_, PyFloat r) -> do
          appendFile mCode ("\tlw $t" ++ show treg ++ ", " ++ prettyPyVar lhs ++ "\n")
          let treg' = treg + 1
          appendFile mCode ("\tmtc1 $t" ++ show treg ++ ", $f" ++ show freg ++ "\n")
          appendFile mCode ("\tcvt.s.w $f" ++ show freg ++ ", $f" ++ show freg ++ "\n")
          let freg' = freg + 1
          appendFile mCode ("\tli.s $f" ++ show freg' ++ ", " ++ show r ++ "\n")
          appendFile mCode ("\tad.s $f" ++ show freg ++ ", $f" ++ show freg ++ ", $f" ++ show freg' ++ "\n")
          putStrLn "Add operation: _ + PyFloat"
        
        -- 8. Literal + var  
        (PyInt l, _) -> do
          appendFile mCode ("\taddi $t" ++ show treg ++ ", $0, " ++ show l ++ "\n")
          let treg' = treg + 1
          appendFile mCode ("\tlw $t" ++ show treg' ++ ", " ++ prettyPyVar rhs ++ "\n")
          let treg'' = treg' + 1
          appendFile mCode ("\tadd $t" ++ show treg'' ++ ", $t" ++ show treg ++ ", $t" ++ show treg' ++ "\n")
          putStrLn "Add operation: PyInt + _"
        
        (PyFloat l, _) -> do
          appendFile mCode ("\tli.s $f" ++ show freg ++ ", " ++ show l ++ "\n")
          let freg' = freg + 1
          appendFile mCode ("\tlw $t" ++ show treg ++ ", " ++ prettyPyVar rhs ++ "\n")
          appendFile mCode ("\tmtc1 $t" ++ show treg ++ ", $f" ++ show freg' ++ "\n")
          appendFile mCode ("\tcvt.s.w $f" ++ show freg' ++ ", $f" ++ show freg' ++ "\n")
          appendFile mCode ("\tadd.s $f" ++ show freg ++ ", $f" ++ show freg ++ ", $f" ++ show freg' ++ "\n")
          putStrLn "Add operation: PyFloat + _"
        
        -- 9. Default: var + var
        (_, _) -> do
          appendFile mCode ("\tlw $t" ++ show treg ++ ", " ++ prettyPyVar lhs ++ "\n")
          let treg' = treg + 1
          appendFile mCode ("\tlw $t" ++ show treg' ++ ", " ++ prettyPyVar rhs ++ "\n")
          let treg'' = treg' + 1
          appendFile mCode ("\tadd $t" ++ show treg'' ++ ", $t" ++ show treg ++ ", $t" ++ show treg' ++ "\n")
          putStrLn "Add operation: _ + _"
      
      putStrLn "Add operation detected"

-- function that translates a simple conditional statement
translateIfChain :: PyIfChain -> FilePath -> FilePath -> Int -> IO Int
translateIfChain ifchain mData mCode labelId = do
  let endLabel = "end_if_" ++ show labelId
  
  -- === IF BLOCK ===
  let elif1Label = if null (elifs ifchain) 
                   then endLabel 
                   else "elif1_" ++ show labelId
  emitConditionJump (ifCond ifchain) elif1Label mCode
  treg1 <- emitBody (ifBody ifchain) mData mCode labelId
  appendFile mCode ("\tj " ++ endLabel ++ "\n\n")
  
  -- === ELIF BLOCKS ===
  treg2 <- translateElifs (elifs ifchain) mData mCode treg1 endLabel
  
  -- === ELSE BLOCK ===
  case elseBody ifchain of
    Just elseStmts -> do
      appendFile mCode ("else_" ++ show labelId ++ ":\n")
      _ <- emitBody elseStmts mData mCode treg2
      return (labelId + 1)
    Nothing -> return (labelId + 1)

-- helper function that translates elif blocks
translateElifs :: [(PyCond, [PyStmt])] -> FilePath -> FilePath -> Int -> String -> IO Int
translateElifs [] _ _ treg _ = do
  putStrLn "DEBUG: No elifs"  -- << ADD
  return treg

translateElifs ((cond, body):rest) mData mCode treg endLabel = do
  let thisLabel = "elif_" ++ show treg
  appendFile mCode (thisLabel ++ ":\n")
  putStrLn $ "DEBUG: Writing elif label: " ++ thisLabel  -- << ADD
  
  emitConditionJump cond endLabel mCode  -- CONDITION JUMP FIRST!
  treg' <- emitBody body mData mCode treg
  appendFile mCode ("\tj " ++ endLabel ++ "\n\n")
  putStrLn $ "DEBUG: Elif body done, treg'=" ++ show treg'  -- << ADD
  
  translateElifs rest mData mCode treg' endLabel

-- function that translates conditional jump
emitConditionJump :: PyCond -> String -> FilePath -> IO ()
emitConditionJump (PyCond lhs cmp rhs) falseLabel mCode = do
  -- Load LHS and RHS into registers $t0, $t1
  loadVarToReg lhs "$t0" mCode
  loadVarToReg rhs "$t1" mCode
  
  -- Emit branch based on comparison (negated for "if" logic)
  case cmp of
    PyEQ  -> appendFile mCode $ "\tbeq $t0, $t1, " ++ falseLabel ++ "\n"     -- Jump if EQUAL (false)
    PyNEQ -> appendFile mCode $ "\tbne $t0, $t1, L_true_" ++ falseLabel ++ "\n"  -- Jump if NOT EQUAL (true falls through)
    PyLT  -> appendFile mCode $ "\tblt $t0, $t1, L_true_" ++ falseLabel ++ "\n"  -- Jump if LESS (true)
    PyLTE -> appendFile mCode $ "\tble $t0, $t1, L_true_" ++ falseLabel ++ "\n"
    PyGT  -> appendFile mCode $ "\tbgt $t0, $t1, L_true_" ++ falseLabel ++ "\n"
    PyGTE -> appendFile mCode $ "\tbge $t0, $t1, L_true_" ++ falseLabel ++ "\n"

-- function that translates code within the body of a conditional statement
emitBody :: [PyStmt] -> FilePath -> FilePath -> Int -> IO Int
emitBody [] _ _ treg = return treg  -- No statements, no regs used

emitBody [] mData mCode treg = do
  appendFile mCode "\tnop\n"  -- placeholder for empty body
  return treg

emitBody (stmt:rest) mData mCode treg = do
  case stmt of
    PyPrint var -> do
      translatePrint (prettyPyVar var) mData mCode treg
      emitBody rest mData mCode (treg + 1)
    PyAssign name val -> do
      translateVariable name val mData mCode treg 0  -- freg=0 for now
      emitBody rest mData mCode (treg + 1)
    _ -> putStrLn "emitBody: Unsupported stmt" >> emitBody rest mData mCode treg

-- Fixed empty chain
emptyChain :: PyIfChain
emptyChain = PyIfChain (PyCond Unknown PyEQ Unknown) [] [] Nothing 0

-- Add line to body
addLineToChain :: PyIfChain -> String -> PyIfChain
addLineToChain chain line = 
  chain { ifBody = parseStmt line : ifBody chain }

-- Add elif
addElifToChain :: PyIfChain -> PyCond -> PyIfChain
addElifToChain chain cond = 
  chain { elifs = (cond, ifBody chain) : elifs chain, ifBody = [] }

-- Set else body  
setElseBody :: PyIfChain -> [PyStmt] -> PyIfChain
setElseBody chain stmts = 
  chain { elseBody = Just stmts }

-- Parse single stmt (simple version)
parseStmt :: String -> PyStmt     -- Now pure!
parseStmt line = 
  case extractVariable line of
    Just (name, val) -> PyAssign name val
    Nothing -> PyPrint (PyStr (extractStringPure line))
  where
    extractStringPure :: String -> String
    extractStringPure trimmed = 
      let extracted = maybe trimmed id (stripPrefix "print(" trimmed)
          noParen = init (noTrailingSpace extracted)
      in noLeadingSpace noParen

-- Parse condition (simple)
parseCond :: String -> PyCond
parseCond line = 
  -- Extract "x > 5" → PyCond (PyStr "x") PyGT (PyInt 5)
  PyCond (PyStr "x") PyGT (PyInt 5)  -- Placeholder for now

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
    writeMipsDefault mipsData mipsCode mipsFinal
    
    -- throw error if file is not found or is empty
    contents <- readFile python `catch` \e -> isFile python e
    isEmptyFile python
    
    -- open Python file handle for line-by-line reading
    withFile python ReadMode $ \h -> do
        loop h mipsData mipsCode strCount tregCount fregCount emptyChain 0  -- start the while loop
    
    -- write end program logic into code.txt
    appendFile mipsCode "\n\t; END PROGRAM"
    appendFile mipsCode "\n\tjr $ra"
    putStrLn "Writing to data.txt and code.txt successful"
    
    -- merge the two .txt files into assembly.s (FINAL STEP OF THE WHOLE PROGRAM)
    mergeMips mipsData mipsCode mipsFinal
    putStrLn "Writing to assembly.s successful"

-- WHILE NOT EOF LOOP (continues until end of test.py)
loop :: Handle -> String -> String -> Int -> Int -> Int -> PyIfChain -> Int -> IO ()
loop h mipsData mipsCode strCount tregCount fregCount chain labelCount = do
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
            ('#':_) -> loop h mipsData mipsCode strCount tregCount fregCount chain labelCount  -- comment line, skip
            _       -> do  -- non-comment, process normally
                if (isEmpty trimmedLine) then
                    putStrLn $ "Empty line skipped"
                else do putStr $ ""
            
        -- trim whitespace and find whether "print" is a prefix of the full string
        let isPrint = "print(" `isPrefixOf` trimmedLine && not (isSpecial trimmedLine 6)
        let isArith = isVarInit trimmedLine && any (`isInfixOf` trimmedLine) [" + ", " - ", " * ", " / ", "//", "%"]
        
        -- check conditional statements
        let lowerTrimmed = map toLower trimmedLine
        let isIf = "if " `isPrefixOf` lowerTrimmed && ":" `isSuffixOf` trimmedLine
        let isElif = "elif " `isPrefixOf` lowerTrimmed && ":" `isSuffixOf` trimmedLine  
        let isElse = "else:" `isInfixOf` lowerTrimmed
        
        -- check function declarations and returns
        let isFunc = "def " `isPrefixOf` trimmedLine
        let isFuncReturn = "return " `isPrefixOf` trimmedLine
        
        -- check variable declarations
        let isVar = "=" `isInfixOf` trimmedLine && not isPrint && not isArith -- CHECK IF INFIX CHECK IS REDUNDANT
        
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
                     else if isArith then 2
                     else if (isIf || isElif || isElse) then 3
                     -- else if isCase then 4
                     -- else if isLoop then 5
                     else if isFunc then 6
                     else if isFuncReturn then 7
                     else if isVar then 1
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
                        putStrLn $ "Arithmetic expression: " ++ destVar ++ " = " ++ lhs ++ " " ++ show op ++ " " ++ renderOperand rhsVal
                        translateExpression (PyStr destVar) (PyStr lhs) op rhsVal mipsData mipsCode tregCount' fregCount'
                    Nothing -> return()
            3 -> do
              -- translate conditional statement
              case (isIf, isElif, isElse) of
                  (True, _, _)    -> do
                    putStrLn "IF condition detected"
                    appendFile mipsCode "\t; IF CONDITION\n"
                  (_, True, _)    -> do
                    putStrLn "ELIF condition detected"
                    appendFile mipsCode "\t; ELIF CONDITION\n"
                  (_, _, True)    -> do
                    putStrLn "ELSE condition detected"
                    appendFile mipsCode "\t; ELSE CONDITION\n"
                  _               -> putStrLn "Invalid conditional"
              
              let lineIndent = length (takeWhile isSpace pyLine)
              let lowerTrim = map toLower trimmedLine
              
              if lineIndent > indentLevel chain then do
                let newChain = addLineToChain chain trimmedLine
                loop h mipsData mipsCode strCount tregCount fregCount newChain labelCount
              
              else if "if " `isPrefixOf` lowerTrim then do
                labelCount' <- translateIfChain chain mipsData mipsCode labelCount
                let freshChain = emptyChain { indentLevel = lineIndent, ifCond = parseCond trimmedLine }
                loop h mipsData mipsCode strCount tregCount fregCount freshChain labelCount'
              
              else if "elif " `isPrefixOf` lowerTrim then do
                let newChain = addElifToChain chain (parseCond trimmedLine)
                loop h mipsData mipsCode strCount tregCount fregCount newChain labelCount
              
              else if "else:" `isInfixOf` lowerTrim then do
                let newChain = setElseBody chain []
                loop h mipsData mipsCode strCount tregCount fregCount newChain labelCount
              
              else do
                labelCount' <- translateIfChain chain mipsData mipsCode labelCount
                loop h mipsData mipsCode strCount tregCount fregCount emptyChain labelCount'
            6 -> do
                -- translate function header
                putStrLn $ "Function header (code abstracted)"
            7 -> do
                -- translate function return statement
                putStrLn $ "Function return (code abstracted)"
            -1 -> do
                -- display "untestable operation" message if line is not empty
                if (not(isEmpty trimmedLine)) then
                    putStrLn $ "Encountered invalid/untested operation"
                else return()
            _ -> do
                -- display error message (statement should not be reachable due to prior checking)
                putStrLn $ "Computation error"
        
        -- new line for formatting (EMPTY LINE CHECK MAY BE REDUNDANT)
        if (not(isEmpty trimmedLine)) then appendFile mipsCode ("\n")
        else do putStr ""
        putStr $ "\n"
        
        -- move to the next line using recursion
        loop h mipsData mipsCode strCount' tregCount' fregCount' chain labelCount
