import Data.List (isPrefixOf)   -- needed to identify leading keywords such as "print"
import Data.Char (isSpace)      -- needed to trim leading white space

main :: IO ()
main = do
    -- define a line of code with leading whitespace (Python indentation)
    let pythonLine = noSpace "\tprint(\"Hello World\")"
    
    -- trim whitespace and find whether "print" is a prefix of the full string
    let result = "print" `isPrefixOf` pythonLine  -- result is a boolean variable
    
    -- display whether the line has "print" as a prefix
    if result then
        putStrLn "Print statement identified"
    else
        putStrLn "Not a print statement"

-- simple function that removes leading whitespace from a line of code
-- key benefit: helps eliminate indentation when looking for prefix keywords
noSpace :: String -> String
noSpace = dropWhile isSpace
