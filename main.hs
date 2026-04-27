-- TESTABLE CASES:
-- SIMPLE PRINT STATEMENTS
-- DECLARING VARIABLES (int, float, bool, string)
-- SIMPLE CONDITIONAL STATEMENTS
-- TWO-OPERAND ARITHMETIC
-- TESTABLE FRINGE CASES: Python file does not exist or is empty
import System.IO                            -- file handling
import Control.Monad (unless, mapM_)        -- loop control
import Control.Monad (foldM)
import Control.Exception (catch, IOException, throwIO)
import System.IO.Error (isDoesNotExistError)
import System.Exit (exitFailure)
import Data.List (dropWhileEnd, stripPrefix)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isSpace, toLower)
import qualified Data.Map as Map

import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

-- write data constructors to define Python variables
data PyVar
  = PyInt Integer | PyFloat Float | PyBool Bool | PyStrLit String | PyVarName String | Unknown
  deriving (Eq, Show)

-- write data constructors to define Python comparison operators
data PyCompare
  = PyEQ | PyNEQ | PyLT | PyLTE | PyGT | PyGTE
  deriving Show

-- write data constructors to define Python arithmetic operators
data PyArith
  = PyAdd | PySub | PyMul | PyDiv | PyFloorDiv | PyMod
  deriving Show

-- write data constructors to define Python conditional statements
data PyCond = PyCond PyVar PyCompare PyVar
  deriving Show

-- write data constructors to define full Python statements
data PyStmt
  = PyAssign String PyVar
  | PyPrint PyVar
  | PyArithStmt String PyVar PyArith PyVar
  | PyUnsupported String
  | PyIfStmt PyCond [PyStmt] [(PyCond, [PyStmt])] (Maybe [PyStmt])
  deriving Show

-- declare type for displaying string variable values
type Env = (Map.Map String PyVar, Int)

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

evalToRegister :: PyVar -> IO String
evalToRegister v = do
  case v of
    PyInt i -> do
      return "$t0"
    PyBool b -> do
      return "$t0"
    PyStrLit s -> do
      let label = "strLit_" ++ sanitizeLabel s
      return label
    PyVarName n -> do
      return "$t0"
    PyFloat f -> do
      return "$f0"
    Unknown -> do
      return "$t0"

-------- PARSING SECTION --------

parseValue :: String -> PyVar
parseValue val =
  let cleaned = noTrailingSpace (noLeadingSpace val)
  in case cleaned of
      ('"':xs) | last xs == '"' -> PyStrLit (init xs)
      ('\'':xs) | last xs == '\'' -> PyStrLit (init xs)
      _ ->
        case readMaybe cleaned :: Maybe Integer of
          Just n -> PyInt n
          Nothing ->
            case readMaybe cleaned :: Maybe Float of
              Just f -> PyFloat f
              Nothing ->
                if cleaned == "True" then PyBool True
                else if cleaned == "False" then PyBool False
                else PyVarName cleaned

prettyPyVar :: PyVar -> String
prettyPyVar (PyInt i)      = show i
prettyPyVar (PyFloat f)    = show f
prettyPyVar (PyBool b)     = (if b then "1" else "0")
prettyPyVar (PyStrLit sl)  = sl
prettyPyVar (PyVarName sn) = sn
prettyPyVar Unknown        = "?"

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

parseSimpleExpr :: String -> PyVar
parseSimpleExpr s =
  let t = noLeadingSpace (noTrailingSpace s)
  in case readMaybe t :: Maybe Integer of
      Just n  -> PyInt n
      Nothing ->
        if all isAlphaNumOrUnderscore t
        then PyVarName t
        else Unknown

isAlphaNumOrUnderscore :: Char -> Bool
isAlphaNumOrUnderscore c =
  c `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")

parseCond :: String -> PyCond
parseCond line =
  let clean = noTrailingSpace (noLeadingSpace line)
      stripped =
        if "if " `isPrefixOf` clean then drop 3 clean
        else if "elif " `isPrefixOf` clean then drop 5 clean
        else clean

      bodyRaw =
        if not (null stripped) && last stripped == ':'
        then init stripped
        else stripped
    
      body = stripParens bodyRaw

      ops = ["==","!=","<=",">=","<",">"]

      findOp [] = Nothing
      findOp (o:os)
        | o `isInfixOf` body = Just o
        | otherwise = findOp os

  in case findOp ops of
      Just op ->
        let (l,r) = splitOnce op body
        in PyCond (parseValue l) (fromMaybe PyEQ (parseCompare op)) (parseValue r)
      Nothing -> PyCond Unknown PyEQ Unknown

parseCondVar :: String -> PyVar
parseCondVar s =
  case parseValue s of
    Unknown -> PyVarName s
    v       -> v

splitOnce :: String -> String -> (String, String)
splitOnce op s =
  let (a,b) = breakOn op s
  in (a, drop (length op) b)

breakOn :: String -> String -> (String, String)
breakOn op s = go "" s
  where
    go acc xs
      | op `isPrefixOf` xs = (reverse acc, xs)
      | null xs = (reverse acc, "")
      | otherwise = go (head xs : acc) (tail xs)

stripParens :: String -> String
stripParens s =
  let t = noLeadingSpace (noTrailingSpace s)
  in if not (null t) && head t == '(' && last t == ')'
     then init (tail t)
     else t
      
-------- INDENTATION & STATEMENT PARSING --------

-- function that counts indentation level (number of leading spaces)
getIndent :: String -> Int
getIndent = length . takeWhile isSpace

-- function that parses entire Python file into AST
parseProgram :: [String] -> [PyStmt]
parseProgram lines = fst (parseBlock 0 lines)

-- function that parses a block of code based on indentation
-- key benefit: naturally handles Python's indentation rules
parseBlock :: Int -> [String] -> ([PyStmt], [String])
parseBlock _ [] = ([], [])

parseBlock indent allLines@(line:rest)
  | isEmpty trimmed = parseBlock indent rest
  | currentIndent < indent = ([], allLines)
  | otherwise =
      let (stmt, remaining1) = parseStmt indent allLines
          (stmts, remaining2) = parseBlock indent remaining1
      in (stmt : stmts, remaining2)
  where
    currentIndent = getIndent line
    trimmed = noLeadingSpace line

-- function that parses a single Python statement
parseStmt :: Int -> [String] -> (PyStmt, [String])
parseStmt indent (line:rest)
  | isIfLine trimmed = parseIf indent (line:rest)
  {- | isPrintLine trimmed =
    case extractStringPure trimmed of
      Just str ->
        (PyPrint (PyStrLit str), rest)
      Nothing ->
        (PyPrint (parseValue (extractPrint trimmed)), rest)-} -- MAY NEED TO CHECK PY_STR_LIT vs. PY_VAR_NAME HERE
  | isArithLine trimmed =
      case extractOperands trimmed of
        Just (d,l,o,r) -> (PyArithStmt d l o r, rest)
        Nothing -> (PyUnsupported trimmed, rest)
  | isPrintLine trimmed =
      let arg = parseValue (extractPrint trimmed)
      in (PyPrint arg, rest)
  | isVarInit trimmed =
      case extractVariable trimmed of
        Just (name, val) -> (PyAssign name val, rest)
        Nothing -> (PyUnsupported trimmed, rest)
  | otherwise = (PyUnsupported trimmed, rest)
  where
    trimmed = noLeadingSpace line
    
    isArithLine :: String -> Bool
    isArithLine s =
      any (`isInfixOf` s) ["+", "-", "*", "/", "//", "%"]

-------- CONDITIONAL PARSING --------

parseIf :: Int -> [String] -> (PyStmt, [String])
parseIf indent (l:ls) =
  let cond = parseCond l
      (ifBody, rest1) = parseBlock (indent + 4) ls
      (elifs, elseBody, rest2) = parseElifs indent rest1
  in (PyIfStmt cond ifBody elifs elseBody, rest2)

parseElifs :: Int -> [String]
           -> ([(PyCond,[PyStmt])], Maybe [PyStmt], [String])
parseElifs indent all@(l:ls)
  | "elif " `isPrefixOf` trim =
      let cond = parseCond l
          (body, r1) = parseBlock (indent + 4) ls
          (el, eb, r2) = parseElifs indent r1
      in ((cond,body):el, eb, r2)

  | "else:" `isPrefixOf` trim =
      let (body, r1) = parseBlock (indent + 4) ls
      in ([], Just body, r1)

  | otherwise = ([], Nothing, all)

  where trim = noLeadingSpace l
  
parseElifs _ [] = ([], Nothing, [])

-------- VARIABLE EXTRACTION --------

extractVariable :: String -> Maybe (String, PyVar)
extractVariable s =
  case words s of
    (n:"=":rest) -> Just (n, parseValue (unwords rest))
    _ -> Nothing

extractPrint :: String -> String
extractPrint s =
  let inside = dropWhile (/= '(') s
      content = takeWhile (/= ')') (drop 1 inside)
  in noLeadingSpace (noTrailingSpace content)

-- helper checks
isIfLine s = "if " `isPrefixOf` s
isElifLine s = "elif " `isPrefixOf` s
isElseLine s = "else:" `isPrefixOf` s
isPrintLine s = "print(" `isPrefixOf` s

-- function that extracts operands of an arithmetic expression
extractOperands :: String -> Maybe (String, PyVar, PyArith, PyVar)
extractOperands line =
  case words line of
    (dest : "=" : rest) ->
      let (lhsParts, op:rhsParts) = break isOp rest
          opStr = op
          lhs = unwords lhsParts
          rhs = unwords rhsParts
      in do
        arithOp <- parseArith opStr
        return (dest, parseSimpleExpr lhs, arithOp, parseSimpleExpr rhs)
    _ -> Nothing
  where
    isOp x = x `elem` ["+", "-", "*", "/", "//", "%"]

trim = noLeadingSpace . noTrailingSpace

-- function that extracts a string from a print statement
extractStringPure :: String -> Maybe String
extractStringPure line =
  let inner = dropWhile (/= '(') line
      content = takeWhile (/= ')') (drop 1 inner)
      trimmed = noLeadingSpace (noTrailingSpace content)
  in
    if isQuotedString trimmed
      then Just (stripQuotes trimmed)
      else Nothing

isQuotedString :: String -> Bool
isQuotedString s =
  (head s == '"' && last s == '"') ||
  (head s == '\'' && last s == '\'')

stripQuotes :: String -> String
stripQuotes s = init (tail s)

sanitizeLabel :: String -> String
sanitizeLabel =
  map (\c -> if c `elem` " ,.!?:;'\"()[]{}-" then '_' else c)

translateStmt :: Env -> FilePath -> FilePath -> PyStmt -> IO Env
translateStmt env mData mCode stmt =
  case stmt of
    PyPrint v -> do
      case v of
    
        PyStrLit s -> do
          let label = "strLit_" ++ sanitizeLabel s
          appendFile mData (label ++ ": .asciiz \"" ++ s ++ "\"\n")
    
          appendFile mCode "\t# PRINT STRING\n"
          appendFile mCode "\tli $v0, 4\n"
          appendFile mCode ("\tla $a0, " ++ label ++ "\n")
    
        PyInt _ -> do
          appendFile mCode "\t# PRINT INTEGER\n"
          loadVarToReg v "t0" mCode
          appendFile mCode "\tli $v0, 1\n"
          appendFile mCode "\tmove $a0, $t0\n"
    
        PyBool _ -> do
          appendFile mCode "\t# PRINT BOOLEAN\n"
          loadVarToReg v "t0" mCode
          appendFile mCode "\tli $v0, 1\n"
          appendFile mCode "\tmove $a0, $t0\n"
    
        PyFloat _ -> do
          appendFile mCode "\t# PRINT FLOAT\n"
          loadVarToReg v "f0" mCode
          appendFile mCode "\tli $v0, 2\n"
          appendFile mCode "\tmov.s $f12, $f0\n"
    
        PyVarName _ -> do
          appendFile mCode "\t# PRINT VARIABLE\n"
          loadVarToReg v "t0" mCode
          appendFile mCode "\tli $v0, 1\n"
          appendFile mCode "\tmove $a0, $t0\n"
    
      appendFile mCode "\tsyscall\n\n"
      return env

    PyAssign n v -> do
      -- reg <- evalToRegister v mData mCode
      let (envMap, counter) = env
      let envMap' = Map.insert n v envMap
  
      -- appendFile mData "# ASSIGNMENT FROM TRANSLATE_STMT "
      case v of
        PyStrLit _ -> appendFile mData (n ++ ":\t.asciiz \"" ++ prettyPyVar v ++ "\"\t\t\t# STRING DECLARATION\n")
        PyInt _ -> appendFile mData (n ++ ":\t.word " ++ prettyPyVar v ++ "\t\t\t# INTEGER DECLARATION\n")
        PyBool _ -> appendFile mData (n ++ ":\t.word " ++ prettyPyVar v ++ "\t\t\t# BOOLEAN DECLARATION\n")
        PyFloat _ -> appendFile mData (n ++ ":\t.float " ++ prettyPyVar v ++ "\t\t\t# FLOAT DECLARATION\n")
        PyVarName x -> do
          -- load value from existing variable x into register
          loadVarToReg (PyVarName x) "t0" mCode
    
          -- store into destination variable n
          appendFile mData (n ++ ":\t.word 0 \t# VARIABLE = VARIABLE\n")
          appendFile mCode ("\t# VARIABLE = VARIABLE\n")
          appendFile mCode ("\tsw $t0, " ++ n ++ "\n")
    
        Unknown ->
          appendFile mData (n ++ ":\t.word 0\n")
          
      return (envMap', counter)
    
    PyArithStmt dest lhs op rhs -> do
      appendFile mCode "\t# ARITHMETIC OPERATION\n"
    
      let r1 = "t0"
      let r2 = "t1"
      let rd = "t2"
    
      loadVarToReg lhs r1 mCode
      loadVarToReg rhs r2 mCode
    
      emitArith op r1 r2 rd mCode
    
      let (envMap, c) = env
      if Map.member dest envMap
        then return ()
        else appendFile mData (dest ++ ": .word 0\t\t\t# USED FOR ARITHMETIC OPERATION\n")
      appendFile mCode ("\tsw $" ++ rd ++ ", " ++ dest ++ "\t\t\t\t# STORE RESULT\n\n")
      return env

    PyIfStmt cond ifBody elifs elseBody -> do
      let (envMap, labelId) = env
    
      let endLabel = "end_if_" ++ show labelId
      let newLabelId = labelId + 1
      
      -- check if condition
      appendFile mCode ("\t# IF CONDITION\n")
    
      -- emit first condition jump
      emitCondJump cond (firstLabel elifs elseBody labelId) mCode
    
      -- translate IF body
      (envMap1, _) <- foldM
        (\(e, c) stmt -> do
            e' <- translateStmt (e, c) mData mCode stmt
            return e'
        )
        (envMap, newLabelId)
        ifBody
    
      appendFile mCode ("\tj " ++ endLabel ++ "\n\n")
    
      -- translate ELIF chain
      (envMap2, finalLabelId) <- foldM
        (\(e, c) (i, (cond, body)) -> do
            (e', c') <- translateElif (e, c) mData mCode endLabel labelId (length elifs) (i, (cond, body))
            return (e', c')
        )
        (envMap1, newLabelId)
        (zip [0..] elifs)
    
      -- translate ELSE
      (envMapFinal, finalCounter) <- case elseBody of
        Just body -> do
          appendFile mCode ("else_" ++ show labelId ++ ":\n")
          (eFinal, cFinal) <- foldM
            (\(e, c) stmt -> do
                e' <- translateStmt (e, c) mData mCode stmt
                return e'
            )
            (envMap2, newLabelId)
            body
    
          appendFile mCode ("\tj " ++ endLabel ++ "\n")
          return (eFinal, cFinal)
    
        Nothing ->
          return (envMap2, newLabelId)
    
      appendFile mCode (endLabel ++ ":\n")
    
      return (envMapFinal, finalCounter)

    PyUnsupported s -> do
      appendFile mCode ("\t# unsupported: " ++ s ++ "\n")
      return env

firstElifLabel :: [(PyCond, [PyStmt])] -> Maybe [PyStmt] -> String
firstElifLabel [] (Just _) = "else"
firstElifLabel [] Nothing  = "end_if"
firstElifLabel _  _        = "elif_0"


nextLabel :: Int -> Int -> String -> String
nextLabel i len labelId
  | i + 1 < len = "elif_" ++ show (i + 1) ++ "_" ++ show labelId
  | otherwise    = "else_" ++ show labelId

translateVariable :: String -> PyVar -> FilePath -> FilePath -> IO ()
translateVariable name val mData mCode =
  case val of
    PyInt i ->
      appendFile mData (name ++ ": .word " ++ show i ++ "\n")

    PyBool b ->
      appendFile mData (name ++ ": .word " ++ (if b then "1" else "0") ++ "\n")

    PyFloat f ->
      appendFile mData (name ++ ": .float " ++ show f ++ "\n")

    PyStrLit s ->
      appendFile mData (name ++ ": .asciiz \"" ++ s ++ "\"\n")
    
    PyVarName s ->
      appendFile mData (name ++ ": .asciiz \"" ++ s ++ "\"\n")

    Unknown ->
      appendFile mData (name ++ ": .word 0\n")

-------- CONDITIONAL STATEMENTS --------

-- function that translates a conditional statement
translateIf :: Env -> PyCond -> [PyStmt] -> [(PyCond, [PyStmt])] -> Maybe [PyStmt]
            -> FilePath -> FilePath -> Int -> IO Env
translateIf env cond ifBody elifs elseBody mData mCode labelId = do

  let endLabel = "end_if_" ++ show labelId
      elseLabel = "else_" ++ show labelId

  -- IF
  appendFile mCode ("# IF CONDITION\n")
  let failTarget =
        case elifs of
            [] -> case elseBody of
                    Nothing -> endLabel
                    Just _  -> "else_" ++ show labelId
            _  -> "elif_0_" ++ show labelId

  emitCondJump cond failTarget mCode

  env1 <- foldM (\e s -> translateStmt e mData mCode s) env ifBody
  appendFile mCode ("\tj " ++ endLabel ++ "\n\n")

  -- ELIF chain (ONLY emit code, do NOT execute)
  let totalElifs = length elifs
  env2 <- foldM
    (\e pair -> translateElif e mData mCode endLabel labelId totalElifs pair)
    env1
    (zip [0..] elifs)

  -- ELSE
  env3 <- case elseBody of
    Just body -> do
      appendFile mCode (elseLabel ++ ":\n")
      foldM (\e s -> translateStmt e mData mCode s) env2 body
    Nothing -> return env2

  appendFile mCode (endLabel ++ ":\n")
  return env3
  
firstLabel :: [(PyCond, [PyStmt])] -> Maybe [PyStmt] -> Int -> String
firstLabel [] (Just _) i = "else_" ++ show i
firstLabel [] Nothing  i = "end_if_" ++ show i
firstLabel _ _ i        = "elif_0_" ++ show i

-- helper for elif translation
translateElif :: Env -> FilePath -> FilePath -> String -> Int
              -> Int -> (Int, (PyCond, [PyStmt])) -> IO Env
translateElif env mData mCode endLabel baseId totalElifs (i, (cond, body)) = do

  let label = "elif_" ++ show i ++ "_" ++ show baseId
  appendFile mCode (label ++ ":\n")

  let nextLabel =
        if i + 1 < totalElifs
        then "elif_" ++ show (i + 1) ++ "_" ++ show baseId
        else "else_" ++ show baseId

  emitCondJump cond nextLabel mCode

  env' <- foldM (\e s -> translateStmt e mData mCode s) env body

  appendFile mCode ("\tj " ++ endLabel ++ "\n\n")
  return env'

emitCondJump :: PyCond -> String -> FilePath -> IO ()
emitCondJump (PyCond lhs cmp rhs) failLabel mCode = do
  loadVarToReg lhs "t0" mCode
  loadVarToReg rhs "t1" mCode
  case cmp of
    PyEQ  -> appendFile mCode ("\tbne $t0, $t1, " ++ failLabel ++ "\n")
    PyNEQ -> appendFile mCode ("\tbeq $t0, $t1, " ++ failLabel ++ "\n")
    PyLT  -> appendFile mCode ("\tbge $t0, $t1, " ++ failLabel ++ "\n")
    PyLTE -> appendFile mCode ("\tbgt $t0, $t1, " ++ failLabel ++ "\n")
    PyGT  -> appendFile mCode ("\tble $t0, $t1, " ++ failLabel ++ "\n")
    PyGTE -> appendFile mCode ("\tblt $t0, $t1, " ++ failLabel ++ "\n")

emitArith :: PyArith -> String -> String -> String -> FilePath -> IO ()
emitArith op r1 r2 dest mCode =
  case op of
    -- translate addition operation
    PyAdd ->
      appendFile mCode ("\tadd $" ++ dest ++ ", $" ++ r1 ++ ", $" ++ r2 ++ "\t\t\t# ADDITION\n")

    -- translate subtraction operation
    PySub ->
      appendFile mCode ("\tsub $" ++ dest ++ ", $" ++ r1 ++ ", $" ++ r2 ++ "\t\t\t# SUBTRACTION\n")

    -- translate multiplication operation
    PyMul ->
      appendFile mCode ("\tmul $" ++ dest ++ ", $" ++ r1 ++ ", $" ++ r2 ++ "\t\t\t# MULTIPLICATION\n")

    -- translate standard division operation
    PyDiv -> do
      appendFile mCode ("\tdiv $" ++ r1 ++ ", $" ++ r2 ++ "\t\t\t# STANDARD DIVISION\n")
      appendFile mCode ("\tmflo $" ++ dest ++ "\t\t\t\t# MOVE QUOTIENT\n")
      appendFile mCode ("\tmfhi $" ++ r2 ++ "\t\t\t\t# MOVE REMAINDER\n")

    -- translate floor divide operation
    PyFloorDiv -> do
      appendFile mCode ("\tdiv $" ++ r1 ++ ", $" ++ r2 ++ "\t\t\t# FLOOR DIVISION\n")
      appendFile mCode ("\tmflo $" ++ dest ++ "\t\t\t\t# MOVE QUOTIENT (NO NEED FOR REMAINDER)\n")

    -- translate modulo operation
    PyMod -> do
      appendFile mCode ("\tdiv $" ++ r1 ++ ", $" ++ r2 ++ "\t\t\t# MODULO\n")
      appendFile mCode ("\tmfhi $" ++ dest ++ "\t\t\t\t# MOVE REMAINDER (NO NEED FOR QUOTIENT)\n")

-- helper function for formatting registers
formatReg :: String -> String
formatReg r =
  if head r == '$' then r else "$" ++ r

-- helper function that renders variables and registers
loadVarToReg :: PyVar -> String -> FilePath -> IO ()
loadVarToReg var reg mCode = do
  let isFloatReg = head reg == 'f'
  case var of
    -- translate integer type
    PyInt i ->
      if isFloatReg then do
        appendFile mCode $ "\tli $t9, " ++ show i ++ "\n"
        appendFile mCode $ "\tmtc1 $t9, $" ++ reg ++ "\n"
        appendFile mCode $ "\tcvt.s.w $" ++ reg ++ ", $" ++ reg ++ "\n"
      else
        appendFile mCode $ "\tli $" ++ reg ++ ", " ++ show i ++ "\n"

    -- translate float type
    PyFloat f ->
      if isFloatReg then
        appendFile mCode $ "\tli.s $" ++ reg ++ ", " ++ show f ++ "\n"
      else do
        appendFile mCode $ "\tli.s $f9, " ++ show f ++ "\n"
        appendFile mCode $ "\tmfc1 $" ++ reg ++ ", $f9\n"

    -- translate boolean type
    PyBool b ->
      appendFile mCode $ "\tli $" ++ reg ++ ", " ++ (if b then "1" else "0") ++ "\n"

    -- translate string literal
    PyStrLit s -> do
      appendFile mCode $ "\tla $" ++ reg ++ ", str_const_" ++ filter (/= ' ') s ++ "\n"
      
    -- translate variable name
    PyVarName name ->
      if isFloatReg then do
        appendFile mCode $ "\tlw $t9, " ++ name ++ "\n"
        appendFile mCode $ "\tmtc1 $t9, $" ++ reg ++ "\n"
        appendFile mCode $ "\tcvt.s.w $" ++ reg ++ ", $" ++ reg ++ "\n"
      else
        appendFile mCode $ "\tlw $" ++ reg ++ ", " ++ name ++ "\n"

    -- translate unknown type
    Unknown ->
      appendFile mCode $ "\tli $" ++ reg ++ ", 0\n"

main :: IO ()
main = do
  let python = "test.py"
  let mData = "data.txt"
  let mCode = "code.txt"
  let mFinal = "assembly.s"

  writeFile mData "\t.data\n"
  writeFile mCode "\t.text\n.globl main\nmain:\n"

  contents <- readFile python `catch` \e ->
    if isDoesNotExistError e then do
      putStrLn "File not found"
      exitFailure
    else throwIO e

  let ast = parseProgram (lines contents)
  print ast

  _ <- foldM (\e stmt -> translateStmt e mData mCode stmt)
           (Map.empty, 0)
           ast
  appendFile mCode "\n\t# END PROGRAM"
  appendFile mCode "\n\tjr $ra\n"

  dataContents <- readFile mData
  codeContents <- readFile mCode
  writeFile mFinal (dataContents ++ "\n" ++ codeContents)

  putStrLn "Writing to assembly.s successful"
  
