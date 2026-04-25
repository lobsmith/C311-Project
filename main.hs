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
  = PyInt Integer
  | PyFloat Float
  | PyBool Bool
  | PyStr String
  | Unknown
  deriving (Eq, Show)

-- write data constructors to define Python comparison operators
data PyCompare
  = PyEQ | PyNEQ | PyLT | PyLTE | PyGT | PyGTE
  deriving Show

-- write data constructors to define Python conditional statements
data PyCond = PyCond PyVar PyCompare PyVar
  deriving Show

-- write data constructors to define full Python statements (UPDATED)
data PyStmt
  = PyAssign String PyVar
  | PyPrint PyVar
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

evalToRegister :: PyVar -> FilePath -> FilePath -> IO String
evalToRegister v mData mCode =
  case v of
    PyInt i -> do
      let r = "$t0"
      appendFile mCode ("\tli " ++ r ++ ", " ++ show i ++ "\n")
      return r

    PyBool b -> do
      let r = "$t0"
      appendFile mCode ("\tli " ++ r ++ ", " ++ (if b then "1" else "0") ++ "\n")
      return r

    PyStr s -> do
      let label = "str_const"
      appendFile mData (label ++ ": .asciiz " ++ s ++ "\n")
      return "$a0"

    PyFloat f -> do
      let r = "$f0"
      appendFile mCode ("\tl.s " ++ r ++ ", " ++ show f ++ "\n")
      return r

    Unknown -> do
      appendFile mCode "\tli $t0, 0\n"
      return "$t0"

-------- PARSING SECTION --------

parseValue :: String -> PyVar
parseValue val =
  let trimmed = noTrailingSpace (noLeadingSpace val)
  in case trimmed of
      "True"  -> PyBool True
      "False" -> PyBool False
      _ ->
        case readMaybe trimmed :: Maybe Integer of
          Just i -> PyInt i
          Nothing ->
            case readMaybe trimmed :: Maybe Float of
              Just f -> PyFloat f
              Nothing ->
                if length trimmed >= 2 &&
                   ((head trimmed == '"' && last trimmed == '"') ||
                    (head trimmed == '\'' && last trimmed == '\''))
                then PyStr (init (tail trimmed))
                else Unknown

prettyPyVar :: PyVar -> String
prettyPyVar (PyInt i) = show i
prettyPyVar (PyFloat f) = show f
prettyPyVar (PyBool b) = map toLower (show b)
prettyPyVar (PyStr s) = s
prettyPyVar Unknown = "?"

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
  | isPrintLine trimmed =
      (PyPrint (PyStr (extractStringPure trimmed)), rest)
  | isVarInit trimmed =
      case extractVariable trimmed of
        Just (name, val) -> (PyAssign name val, rest)
        Nothing -> (PyUnsupported trimmed, rest)
  | otherwise = (PyUnsupported trimmed, rest)
  where
    trimmed = noLeadingSpace line

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
  let x = dropWhile (/= '(') s
  in take (length x - 1) (drop 1 x)

-- helper checks
isIfLine s = "if " `isPrefixOf` s
isElifLine s = "elif " `isPrefixOf` s
isElseLine s = "else:" `isPrefixOf` s
isPrintLine s = "print(" `isPrefixOf` s

-- reuse your string extraction logic
extractStringPure :: String -> String
extractStringPure trimmed =
  let extracted = maybe trimmed id (stripPrefix "print(" trimmed)
      noParen = init (noTrailingSpace extracted)
  in noLeadingSpace noParen

--------------------------------------------------------------------------------
-- CONDITIONAL PARSER (CORE FIX)
--------------------------------------------------------------------------------

-- function that parses a full conditional statement (if / elif / else)
{- parseIf :: Int -> [String] -> (PyStmt, [String])
parseIf indent (line:rest) =
  let cond = parseCond line

      -- parse IF body
      (ifBody, afterIf) = parseBlock (indent + 4) rest

      -- parse ELIF and ELSE chains
      (elifs, elseBody, remaining) = parseElifElse indent afterIf

  in (PyIfStmt cond ifBody elifs elseBody, remaining)

-- function that parses elif and else blocks following an if
parseElifElse :: Int -> [String]
              -> ([(PyCond, [PyStmt])], Maybe [PyStmt], [String])
parseElifElse indent allLines@(line:rest)
  | isElifLine trimmed =
      let cond = parseCond line
          (body, afterElif) = parseBlock (indent + 4) rest
          (elifs, elseBody, remaining) = parseElifElse indent afterElif
      in ((cond, body) : elifs, elseBody, remaining)

  | isElseLine trimmed =
      let (body, remaining) = parseBlock (indent + 4) rest
      in ([], Just body, remaining)

  | otherwise = ([], Nothing, allLines)

  where
    trimmed = noLeadingSpace line

parseElifElse _ [] = ([], Nothing, []) -}

--------------------------------------------------------------------------------
-- TRANSLATION ENTRY (REPLACES LOOP-DRIVEN TRANSLATION)
--------------------------------------------------------------------------------

-- function that translates a parsed statement into MIPS
{- translateStmt :: FilePath -> FilePath -> PyStmt -> IO ()
translateStmt mData mCode stmt =
  case stmt of
    PyPrint val ->
      translatePrint (prettyPyVar val) mData mCode 0

    PyAssign name val ->
      translateVariable name val mData mCode 0 0

    PyIfStmt cond ifBody elifs elseBody ->
      translateIf cond ifBody elifs elseBody mData mCode 0

    PyUnsupported s ->
      putStrLn $ "Skipping unsupported: " ++ s -}

translateStmt :: FilePath -> FilePath -> PyStmt -> IO ()
translateStmt mData mCode stmt =
  case stmt of
    PyPrint v -> do
      -- appendFile mData ("var: .asciiz \"" ++ extractStringPure stmt ++ "\"\n")
      appendFile mCode "\t# PRINT STATEMENT\n"

      -- evaluate value into register
      reg <- evalToRegister v mData mCode

      -- move into register based on type
      case v of
        PyFloat _ -> appendFile mCode ("\tmov.s $f12, " ++ reg ++ "\n")
        PyStr _   -> appendFile mCode ("\tla $a0, str_const\n")
        _         -> appendFile mCode ("\tmove $a0, " ++ reg ++ "\n")
        
      -- select syscall type
      case v of
        PyStr _ -> appendFile mCode "\tli $v0, 4\n"
        PyFloat _ -> appendFile mCode "\tli $v0, 2\n"
        _ -> appendFile mCode "\tli $v0, 1\n"

      appendFile mCode "\tsyscall\n\n"

    PyAssign n v -> do
      appendFile mData (n ++ ":\t.word " ++ prettyPyVar v ++ "\n")

    PyIfStmt c ifb elb elseb -> do
      let endLabel = "end_if"
    
      -- heck if condition
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

translatePrint :: String -> FilePath -> FilePath -> IO ()
translatePrint str mData mCode = do
  appendFile mData ("str_label: .asciiz \"" ++ str ++ "\"\n")

  appendFile mCode $
    "\tli $v0, 4\n" ++
    "\tla $a0, str_label\n" ++
    "\tsyscall\n\n"

translateVariable :: String -> PyVar -> FilePath -> FilePath -> IO ()
translateVariable name val mData mCode =
  case val of

    PyInt i ->
      appendFile mData (name ++ ": .word " ++ show i ++ "\n")

    PyBool b ->
      appendFile mData (name ++ ": .word " ++ (if b then "1" else "0") ++ "\n")

    PyFloat f ->
      appendFile mData (name ++ ": .float " ++ show f ++ "\n")

    PyStr s ->
      appendFile mData (name ++ ": .asciiz \"" ++ s ++ "\"\n")

    Unknown ->
      appendFile mData (name ++ ": .word 0\n")



--------------------------------------------------------------------------------
-- CONDITIONAL TRANSLATION (REPLACES translateIfChain)
--------------------------------------------------------------------------------

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
-- translateElif :: FilePath -> FilePath -> String -> (Int, (PyCond, [PyStmt])) -> IO ()
-- translateElif mData mCode endLabel (i, (cond, body)) = do
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

-- helper function that renders variables and registers
loadVarToReg :: PyVar -> String -> FilePath -> IO ()
loadVarToReg var reg mCode = do
  case var of
    PyInt i   -> appendFile mCode $ "\tli $" ++ reg ++ ", " ++ show i ++ "\n"
    PyFloat f -> appendFile mCode $ "\tli.s $" ++ reg ++ ", " ++ show f ++ "\n"
    PyStr s   -> appendFile mCode $ "\tla $" ++ reg ++ ", " ++ s ++ "\n"
    PyBool b  -> appendFile mCode $ "\tli $" ++ reg ++ ", " ++ (if b then "1" else "0") ++ "\n"
    Unknown   -> appendFile mCode $ "\tli $" ++ reg ++ ", 0\n"

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
  appendFile mCode "\n\t; END PROGRAM"
  appendFile mCode "\n\tjr $ra\n"

  dataContents <- readFile mData
  codeContents <- readFile mCode
  writeFile mFinal (dataContents ++ "\n" ++ codeContents)

  putStrLn "Writing to assembly.s successful"
