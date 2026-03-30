import System.IO    -- needed for OpenFile

-- function gets a tuple and returns the second element
-- (which will be written into assembly.s)
--writeSecond :: (a, String) -> IO ()
--writeSecond (_, str) = do
    --appendFile "assembly.s" str

-- read one line of Python code (print("Hello World"))
readLine :: FilePath -> IO()
readLine python = do
    withFile python ReadMode $ \handle -> do
        firstLine <- hGetLine handle
        putStrLn firstLine

-- translate print statement into assembly code and write into mips (very rough)
translatePrint :: String -> FilePath -> IO()
translatePrint str mips = do
    -- write .data translation
    appendFile mips "IN DATA SECTION:\n"
    appendFile mips ("strName: .asciiz \"" ++ str ++ "\"\n")
    
    -- may use or not (yet to be determined)
    let printInstr = ("print(" ++ str ++ ")")
    
    -- write .text translation
    appendFile mips "\nIN TEXT SECTION:\n"
    appendFile mips ("\tla $a0, strName\n" ++ "\tli $v0, 4\n")
    appendFile mips ("\tsyscall\n\n")
    
    putStrLn "Leaving translatePrint function"

main :: IO()
main = do
    let python = "test.py"
    let mips = "assembly.s"
    writeFile mips ""
    
    -- read string from Python file here
    pyContents <- readFile python
    readLine python
    
    let strLiteral = "Python String\\n"
    --let printInstr = "print(" ++ pyString ++ ")"
    --let myTuple = (printInstr, "syscall 4\ntest")
    
    -- write .text and main header to mips file
    --appendFile mips ("\n\t\t.text\n" ++ ".globl main\n" ++ "main:\n")
    
    -- write string logic in mips file (4 is syscall for print string)
    --appendFile mips ("\tla $a0, out_string\n" ++ "\tli $v0, 4\n")
    --appendFile mips ("\tsyscall\n\n")
    
    --handle <- openFile python ReadMode
    --line <- hGetLine handle
    --putStrLn line
    --hClose handle
    
    --writeSecond myTuple
    translatePrint strLiteral mips
    putStrLn "Writing to assembly.s successful"
