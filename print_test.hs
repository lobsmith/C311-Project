-- FULL PROGRAM FOR TRANSLATING print("Hello world")
import System.IO                            -- needed for OpenFile
import Data.List (isPrefixOf)               -- needed to identify leading keywords such as "print"
import Data.Char (isSpace)                  -- needed to trim leading and trailing white space
import Data.List (dropWhileEnd)             -- needed to trim trailing white space
import Data.Char (isPunctuation, isSymbol)  -- needed to check for special characters
import Data.List (delete, stripPrefix)      -- delete removes first occurrence of substring
import Data.List.Split (splitOn)            -- needed for string extraction (concat)
import qualified Data.Text as T             -- (breakOn, append, drop, length)

-- read one line of Python code (print("Hello World"))
readLine :: FilePath -> IO String
readLine py = do
    withFile py ReadMode $ \handle -> do
        firstLine <- hGetLine handle
        return firstLine

-- simple function that removes leading whitespace from a line of code
-- key benefit: helps eliminate indentation when looking for prefix keywords
noLeadingSpace :: String -> String
noLeadingSpace = dropWhile isSpace

-- simple function that removes trailing whitespace from a line of code
noTrailingSpace :: String -> String
noTrailingSpace = dropWhileEnd isSpace

-- simple function that determines whether a line of code is empty
isEmpty :: String -> Bool
isEmpty str = all isSpace str

-- function that removes code that is not a string literal or a variable
-- Purpose: to display a string literal or variable in MIPS without the surrounding Python syntax
extractString :: String -> IO String -- String -> IO String
extractString str = do
    let extracted = maybe str id (stripPrefix "print(" str)
    
    -- ADD CODE TO REMOVE COMMENTS (#)
    
    -- remove trailing space and final parenthesis
    let extracted1 = noTrailingSpace extracted
    return (init extracted1)

-- function that checks for special characters in variable declarations at a certain index
isSpecial :: String -> Int -> Bool
isSpecial str index =
    let char = str !! index in
        if (char == '\"')    -- exclude double quotes, since they are needed for string literals
            then False
        else (isPunctuation char || isSymbol char)
    -- in char /= '\"' && (isPunctuation char || isSymbol char)

-- function that translates a print statement into assembly code and writes into MIPS
translatePrint :: String -> FilePath -> FilePath -> IO()
translatePrint str mData mCode = do
    -- write .data translation
    appendFile mData ("strName: .asciiz \"" ++ str ++ "\"\n")
    
    -- may use or not (YET TO BE DETERMINED)
    -- let printInstr = ("print(" ++ str ++ ")")
    
    -- write .text translation
    appendFile mCode ("\tla $a0, strName\n" ++ "\tli $v0, 4\n")
    appendFile mCode ("\tsyscall\n\n")

    -- display message to terminal
    putStrLn "Leaving translatePrint function"

-- MAIN FUNCTION
main :: IO()
main = do
    -- define file names
    let python = "test.py"
    let mipsData = "data.txt"
    let mipsCode = "code.txt"
    let mipsFinal = "assembly.s"
    
    -- write default code to MIPS data and code sections, separate into two .txt files until done
    writeFile mipsData "\t.data\n"
    writeFile mipsCode ("\t.text\n" ++ "\t.globl main\n" ++ "main:\n")
    
    -- read string from Python file here (DO SOMETHING WITH THIS)
    pyContents <- readFile python
    --readLine python
    
    -- read first line and trim leading whitespace
    pyLine <- readLine python
    let trimmedLine = noLeadingSpace pyLine
    
    -- WILL LIKELY MODIFY TO A WHILE LOOP
    if (isEmpty trimmedLine) then -- OR
        putStrLn $ "Encountered an empty line"
    else do putStr $ ""
    
    -- IF CHARACTER AT trimmedLine INDEX 0 IS '#', THEN IGNORE COMPLETELY AND MOVE TO THE NEXT LINE
    
    -- NEED TO ACCOUNT FOR INDENTATION IN FUTURE TESTS
    -- MAY MAKE isPrint A GENERIC RESULT VARIABLE (INT)
    -- * EACH TYPE OF STATEMENT WILL MAP TO AN INTEGER (i.e. PRINT IS 0, .FORMAT() IS 1, ETC.)
    -- * IN THIS FORMAT, THE VALUE OF isPrint OR result WILL BE TESTED IN A CASE STATEMENT
    -- * MAYBE ASSOCIATE WITH THE CORRECT ASSEMBLY SYSCALLS (4 FOR PRINTING STRING, FOR EXAMPLE)
    
    -- trim whitespace and find whether "print" is a prefix of the full string
    let isPrint = "print(" `isPrefixOf` trimmedLine && not (isSpecial trimmedLine 6) -- result is a boolean variable
    -- && (")" `isPostfixOf` trimmedLine && not (isSpecial trimmedLine (length trimmedLine - 2)))
    
    -- THIS SECTION MAY BECOME A LENGTHY CASE STATEMENT TESTING isPrint/result AT SOME POINT
    -- display whether the line has "print" as a prefix
    if isPrint then do
        -- translate first line into MIPS
        strLiteral <- extractString trimmedLine --"Python String\\n"
        translatePrint strLiteral mipsData mipsCode
        putStrLn "Writing to data.txt and code.txt successful"
    else    -- A LOT OF CODE WILL BE ADDED LATER
        putStrLn "Not a print statement (code abstracted)"
    
    -- _ <- hGetLine python -- Moves the file pointer to the next line
    
    -- write end program logic into code.txt
    appendFile mipsCode "\tjr $ra"
    
    -- merge the two .txt files into assembly.s (FINAL STEP OF THE WHOLE PROGRAM)
    dataContents <- readFile mipsData
    codeContents <- readFile mipsCode
    writeFile mipsFinal (dataContents ++ "\n" ++ codeContents)
    putStrLn "Writing to assembly.s successful"
    
