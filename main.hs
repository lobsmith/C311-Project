-- FULL PROGRAM FOR TRANSLATING MULTIPLE SIMPLE PRINT STATEMENTS AND DECLARING VARIABLES (int, float, bool, string)
import System.IO                            -- needed for OpenFile
import Data.List (isPrefixOf, isInfixOf)    -- needed to identify leading keywords ("print") and assignment operator
import Data.List (delete, stripPrefix)      -- delete removes first occurrence of substring
import Data.List (dropWhileEnd)             -- needed to trim trailing white space
import Data.List.Split (splitOn)            -- needed for string extraction (concat)
import Data.Char (isSpace)                  -- needed to trim leading and trailing white space
import Data.Char (isPunctuation, isSymbol, isDigit)  -- needed to check for special characters
import Data.Typeable                        -- needed to determine the type of a variable (using PyVar)
import qualified Data.Text as T             -- (breakOn, append, drop, length) - [CHECK USE IN PROGRAM AND WHICH COMMANDS IT UTILIZES]
import Control.Monad(unless)                -- needed for function loop
import Text.Read (readMaybe)                -- needed for [ADD COMMENT]

-- read one line of Python code and return it (CHECK WHETHER THIS FUNCITON IS USED, MIGHT DELETE ENTIRELY)
readLine :: FilePath -> IO String
readLine py = do
    withFile py ReadMode $ \handle -> do
        line <- hGetLine handle
        return line

-- declare Python variable types
data PyVar = PyInt Integer | PyFloat Float | PyBool Bool | PyStr String | Unknown deriving Show

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
    
    where
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
                -- Strip EXACTLY one pair of quotes from ends
                if length trimmed >= 2 
                   && (head trimmed == '"' && last trimmed == '"' 
                       || head trimmed == '\'' && last trimmed == '\'')
                then PyStr (take (length trimmed - 2) (drop 1 trimmed))
                else Unknown
        
        -- determine whether variable declaration is valid
        isVarInit :: String -> Bool
        isVarInit trimmed = case words trimmed of
            (var : eq : _) | eq == "="  -> isValidName var
            _                           -> False
        
        -- determine whether variable name is valid
        isValidName :: String -> Bool
        isValidName [] = False
        isValidName (char:trimmed)
            | not (isValidLead char) = False
            | otherwise = all isValidFollow trimmed
            where
                isValidLead char = char `elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
                isValidFollow char = char `elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['_'] ++ ['0'..'9']
        
    -- END OF EXTRACT_VARIABLE

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
    
    -- may use or not (YET TO BE DETERMINED)
    -- let printInstr = ("print(" ++ str ++ ")")
    
    -- write .text translation
    appendFile mCode ("\tla $a0, str" ++ show strCount ++ "\n" ++ "\tli $v0, 4\n")
    appendFile mCode ("\tsyscall\n\n")

    -- display message to terminal
    putStrLn "translatePrint successful"

-- function that translates a variable declaration into MIPS (ONLY INT SO FAR)
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
        let isPrint = "print(" `isPrefixOf` trimmedLine && not (isSpecial trimmedLine 6)
        let isVar = "=" `isInfixOf` trimmedLine && not (isPrint) -- CHECK IF INFIX CHECK IS REDUNDANT
        
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
                     -- else if is### then 2
                     -- else if is### then 3
                     -- else if is### then 4
                     -- else if is### then 5
                     -- else if is### then 6
                     -- else if is### then 7
                     -- else if is### then 8
                     else -1
        
        -- translate line into MIPS based on the type of statement
        case result of
            0 -> do
                -- translate print line into MIPS
                strLiteral <- extractString trimmedLine
                translatePrint strLiteral mipsData mipsCode strCount'
            1 -> do
                maybeVar <- pure $ extractVariable trimmedLine
                case maybeVar of
                    Just (varName, varValue) -> do
                        -- Use varName and varValue
                        putStrLn $ "Var: " ++ varName ++ " = " ++ show varValue
                        translateVariable varName varValue mipsData mipsCode tregCount' fregCount' -- varName and varValue now in scope for whole do block
                    Nothing -> return()
            -1 -> do
                putStrLn $ "Encountered invalid/untested operation or empty line"
            _ -> do
                putStrLn $ "Computation error"
        
        -- move to next line (uses recursion)
        loop h mipsData mipsCode strCount' tregCount' fregCount'
        
