-- TESTABLE CASES:
-- SIMPLE PRINT STATEMENTS WITH STRINGS
-- DECLARING VARIABLES (int, float, bool, string)
-- SIMPLE CONDITIONAL STATEMENTS
-- TESTABLE FRINGE CASES: Python file does not exist or is empty
import System.IO                            -- file handling
import Control.Monad (unless, mapM_)        -- loop control
import Control.Exception (catch, IOException, throwIO)
import System.IO.Error (isDoesNotExistError)
import System.Exit (exitFailure)
import Data.List (dropWhileEnd, stripPrefix)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isSpace, toLower)

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

{- evalToRegister :: PyVar -> FilePath -> FilePath -> IO String
evalToRegister v mData mCode =
  case v of
    PyInt i -> do
      appendFile mCode ("\tli $t0, " ++ show i ++ "\n")
      return "$t0"

    PyBool b -> do
      appendFile mCode ("\tli $t0, " ++ (if b then "1" else "0") ++ "\n")
      return "$t0"

    PyStrLit s -> do
      let label = "strLit_" ++ filter (/= ' ') s
      appendFile mData (label ++ ": .asciiz \"" ++ s ++ "\"\n")
      return label
    
    PyVarName n -> do
      appendFile mCode ("\tlw $t0, " ++ n ++ "\n")
      return "$t0"

    PyFloat f -> do
      appendFile mCode ("\tl.s $f0, " ++ show f ++ "\n")
      return "$f0"

    Unknown -> do
      appendFile mCode "\tli $t0, 0\n"
      return "$t0" -}
evalToRegister :: PyVar -> IO String
evalToRegister v = do
  case v of
    PyInt i -> do
      return "$t0"
    PyBool b -> do
      return "$t0"
    PyStrLit s -> do
      let label = "strLit_" ++ filter (/= ' ') s
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
      token = takeWhile (\c -> c /= ')' && c /= '(' && c /= ',') cleaned
  in
    if token == "True" then PyBool True
    else if token == "False" then PyBool False
    else case readMaybe token :: Maybe Integer of
      Just n -> PyInt n
      Nothing -> case readMaybe token :: Maybe Float of
        Just f -> PyFloat f
        Nothing ->
          if length token >= 2 &&
             ((head token == '"' && last token == '"') ||
              (head token == '\'' && last token == '\''))
          then PyStrLit (init (tail token))
          else PyVarName token

prettyPyVar :: PyVar -> String
prettyPyVar (PyInt i)      = show i
prettyPyVar (PyFloat f)    = show f
prettyPyVar (PyBool b)     = (if b then "1" else "0") -- map toLower (show b)
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

parseCond :: String -> PyCond
parseCond line =
  let clean = noTrailingSpace (noLeadingSpace line)
      stripped =
        if "if " `isPrefixOf` clean then drop 3 clean
        else if "elif " `isPrefixOf` clean then drop 5 clean
        else clean

      body = if not (null stripped) && last stripped == ':'
             then init stripped
             else stripped

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
  | isPrintLine trimmed =
      let arg = parseValue (extractPrint trimmed)
      in (PyPrint arg, rest)
  | isVarInit trimmed =
      case extractVariable trimmed of
        Just (name, val) -> (PyAssign name val, rest)
        Nothing -> (PyUnsupported trimmed, rest)
  | isArithLine trimmed =
      case extractOperands trimmed of
        Just (d,l,o,r) -> (PyArithStmt d l o r, rest)
        Nothing -> (PyUnsupported trimmed, rest)
  | otherwise = (PyUnsupported trimmed, rest)
  where
    trimmed = noLeadingSpace line
    
    isArithLine :: String -> Bool
    isArithLine s =
      case words s of
        (_ : "=" : _ : op : _) -> op `elem` ["+","-","*","/","//","%"]
        _ -> False

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

-- function that extracts operands of a arithmetic expression
extractOperands :: String -> Maybe (String, PyVar, PyArith, PyVar)
extractOperands line =
  case words line of
    (dest : "=" : lhs : op : rhsParts) -> do
      arithOp <- parseArith op
      let rhs = unwords rhsParts

      let lhsVar = parseValue lhs
      let rhsVar = parseValue rhs

      return (dest, lhsVar, arithOp, rhsVar)

    _ -> Nothing

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

translateStmt :: FilePath -> FilePath -> PyStmt -> IO ()
translateStmt mData mCode stmt =
  case stmt of
    PyPrint v -> do
      reg <- evalToRegister v
    
      case v of
        PyStrLit _ -> do
          appendFile mCode "\t# PRINT STATEMENT (STRING LITERAL)\n"
          appendFile mCode "\tli $v0, 4\n"
          appendFile mCode ("\tla $a0, " ++ reg ++ "\n")
        PyInt _ -> do
          appendFile mCode "\t# PRINT STATEMENT (INTEGER)\n"
          appendFile mCode "\tli $v0, 1\n"
          appendFile mCode ("\tmove $a0, " ++ reg ++ "\n")
        PyBool _ -> do
          appendFile mCode "\t# PRINT STATEMENT (BOOLEAN CONVERTED TO INTEGER)\n"
          appendFile mCode "\tli $v0, 1\n"
          appendFile mCode ("\tmove $a0, " ++ reg ++ "\n")
        PyFloat _ -> do
          appendFile mCode "\t# PRINT STATEMENT (FLOAT)\n"
          appendFile mCode "\tli $v0, 2\n"
          appendFile mCode ("\tmov.s $f12, " ++ reg ++ "\n")
        PyVarName _ -> do
          appendFile mCode "\t# PRINT STATEMENT (VARIABLE)\n"
          appendFile mCode "\tli $v0, 1\n"
          appendFile mCode ("\tmove $a0, " ++ reg ++ "\n")
    
      appendFile mCode "\tsyscall\n\n"

    PyAssign n v -> do
      -- reg <- evalToRegister v mData mCode
      appendFile mData "# ASSIGNMENT FROM TRANSLATE_STMT "
      case v of
        PyStrLit _ -> appendFile mData (n ++ ":\t.asciiz \"" ++ prettyPyVar v ++ "\"\n")
        PyInt _ -> appendFile mData (n ++ ":\t.word " ++ prettyPyVar v ++ "\n")
        PyBool _ -> appendFile mData (n ++ ":\t.word " ++ prettyPyVar v ++ "\t\t\t# BOOLEAN DECLARATION\n")
        PyFloat _ -> appendFile mData (n ++ ":\t.float " ++ prettyPyVar v ++ "\n")
        PyVarName x -> do
          -- load value from existing variable x into register
          loadVarToReg (PyVarName x) "t0" mCode
    
          -- store into destination variable n
          appendFile mData (n ++ ":\t.word 0 \t# VARIABLE = VARIABLE\n")
          appendFile mCode ("\t# VARIABLE = VARIABLE\n")
          appendFile mCode ("\tsw $t0, " ++ n ++ "\n")
    
        Unknown ->
          appendFile mData (n ++ ":\t.word 0\n")
    
    PyArithStmt dest lhs op rhs -> do
      appendFile mCode "\t# ARITHMETIC OPERATION\n"
    
      let r1 = "t0"
      let r2 = "t1"
      let rd = "t2"
    
      loadVarToReg lhs r1 mCode
      loadVarToReg rhs r2 mCode
    
      emitArith op r1 r2 rd mCode
    
      appendFile mData (dest ++ ": .word 0\n")
      appendFile mCode ("\tsw " ++ rd ++ ", " ++ dest ++ "\n\n")

    PyIfStmt c ifb elb elseb -> do
      let endLabel = "end_if"
    
      -- check if condition
      appendFile mCode ("\t# IF CONDITION\n")
      emitCondJump c (firstElifLabel elb elseb) mCode
    
      -- translate if body
      mapM_ (translateStmt mData mCode) ifb
      appendFile mCode ("\tj " ++ endLabel ++ "\n\n")
    
      -- translate elif chain
      mapM_
        (\(i, (cond, body)) ->
            let label = "elif_" ++ show i
            in do
              appendFile mCode ("\t# ELIF CONDITION\n")
              appendFile mCode (label ++ ":\n")
    
              emitCondJump cond (nextLabel i elb elseb) mCode
    
              mapM_ (translateStmt mData mCode) body
    
              appendFile mCode ("\tj " ++ endLabel ++ "\n\n")
        )
        (zip [0..] elb)
    
      -- translate else block
      case elseb of
        Just b -> do
          appendFile mCode ("\t# ELSE CONDITION\n")
          appendFile mCode "else:\n"
          mapM_ (translateStmt mData mCode) b
          appendFile mCode ("\tj " ++ endLabel ++ "\n")
        Nothing -> return ()
    
      -- end label
      appendFile mCode (endLabel ++ ":\n")

    PyUnsupported s -> do
      appendFile mCode ("\t# unsupported: " ++ s ++ "\n")

firstElifLabel :: [(PyCond, [PyStmt])] -> Maybe [PyStmt] -> String
firstElifLabel [] (Just _) = "else"
firstElifLabel [] Nothing  = "end_if"
firstElifLabel _  _        = "elif_0"

nextLabel :: Int -> [(PyCond, [PyStmt])] -> Maybe [PyStmt] -> String
nextLabel i elb elseb
  | i + 1 < length elb = "elif_" ++ show (i + 1)
  | otherwise =
      case elseb of
        Just _  -> "else"
        Nothing -> "end_if"

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
translateIf :: PyCond -> [PyStmt] -> [(PyCond, [PyStmt])] -> Maybe [PyStmt] -> FilePath -> FilePath -> Int -> IO ()
translateIf cond ifBody elifs elseBody mData mCode labelId = do
  -- define final label for exiting conditional statement
  let endLabel = "end_if_" ++ show labelId

  -- if condition check and translation
  emitCondJump cond (firstLabel elifs elseBody labelId) mCode
  mapM_ (translateStmt mData mCode) ifBody
  appendFile mCode ("\tj " ++ endLabel ++ "\n\n")

  -- translate elif chain (any amount)
  mapM_
    (translateElif mData mCode endLabel labelId)
    (zip [0..] elifs)

  -- translate else block
  case elseBody of
    Just body -> do
      appendFile mCode ("else_" ++ show labelId ++ ":\n")
      mapM_ (translateStmt mData mCode) body
      appendFile mCode ("\tj " ++ endLabel ++ "\n")
    Nothing -> return ()

  -- end label 
  appendFile mCode (endLabel ++ ":\n")
  
  where
    firstLabel :: [(PyCond, [PyStmt])] -> Maybe [PyStmt] -> Int -> String
    firstLabel [] (Just _) i = "else_" ++ show i
    firstLabel [] Nothing  i = "end_if_" ++ show i
    firstLabel _ _ i        = "elif_0_" ++ show i

-- helper for elif translation
translateElif :: FilePath -> FilePath -> String -> Int -> (Int, (PyCond, [PyStmt])) -> IO ()
translateElif mData mCode endLabel baseId (i, (cond, body)) = do
  -- define first elif label
  let label = "elif_" ++ show i ++ "_" ++ show baseId
  appendFile mCode (label ++ ":\n")
  
  -- define other elif labels if needed
  let nextLabel =
        if i + 1 < length body
        then "elif_" ++ show (i + 1) ++ "_" ++ show baseId
        else maybe ("else_" ++ show baseId) (const endLabel) (Just body)
        
  emitCondJump cond nextLabel mCode
  mapM_ (translateStmt mData mCode) body

  appendFile mCode ("\tj " ++ endLabel ++ "\n\n")

  {- let label = "elif_" ++ show i
  appendFile mCode (label ++ ":\n")
  emitCondJump cond endLabel mCode
  mapM_ (translateStmt mData mCode) body
  appendFile mCode ("\tj " ++ endLabel ++ "\n") -}

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
      appendFile mCode ("\tadd " ++ dest ++ ", " ++ r1 ++ ", " ++ r2 ++ "\n")

    -- translate subtraction operation
    PySub ->
      appendFile mCode ("\tsub " ++ dest ++ ", " ++ r1 ++ ", " ++ r2 ++ "\n")

    -- translate multiplication operation
    PyMul ->
      appendFile mCode ("\tmul " ++ dest ++ ", " ++ r1 ++ ", " ++ r2 ++ "\n")

    -- translate integer division operation
    PyDiv -> do
      appendFile mCode ("\tdiv " ++ r1 ++ ", " ++ r2 ++ "\n")
      appendFile mCode ("\tmflo " ++ dest ++ "\n")

    -- translate floor divide operation
    PyFloorDiv -> do
      appendFile mCode ("\tdiv " ++ r1 ++ ", " ++ r2 ++ "\n")
      appendFile mCode ("\tmflo " ++ dest ++ "\n")

    -- translate modulo operation
    PyMod -> do
      appendFile mCode ("\tdiv " ++ r1 ++ ", " ++ r2 ++ "\n")
      appendFile mCode ("\tmfhi " ++ dest ++ "\n")

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
      -- let label = "str_const_" ++ filter (/= ' ') s
      -- appendFile mCode $ label ++ ": .asciiz \"" ++ s ++ "\"\n"
      -- appendFile mCode $ "\tla $" ++ reg ++ ", " ++ label ++ "\n"
      appendFile mCode $ "\tla $" ++ reg ++ ", str_const_" ++ filter (/= ' ') s ++ "\n"
      -- return ("strLit_" ++ filter (/= ' ') s)

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

  mapM_ (translateStmt mData mCode) ast
  appendFile mCode "\n\t# END PROGRAM"
  appendFile mCode "\n\tjr $ra\n"

  dataContents <- readFile mData
  codeContents <- readFile mCode
  writeFile mFinal (dataContents ++ "\n" ++ codeContents)

  putStrLn "Writing to assembly.s successful"
