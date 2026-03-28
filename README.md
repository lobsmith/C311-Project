# C311-Project
Commands to view and run file in Command Prompt:
1. Clone repository: git clone https://github.com/lobsmith/C311-Project
2. Enter repository: cd C311-Project
3. Running project:  [to be determined]

## Abstract
The general purpose of the program will be to convert a program from a high-level programming language such as C++ into machine code using Haskell, which is a purely functional programming language. The first step would be to convert the program into executable instructions, which would be equivalent to code written in assembly language. From there, the assembly code would be converted into machine language.

## Haskell as a Language

### History & Background
Haskell is a general purpose, purely functional language that first appeared in April 1990. The goal of the language was to create a powerful common standard for pure functional languages with lazy evaluation, which delays the evaluation of an expression until its result is needed. It was named after the late logician Haskell Curry, who was not a primary designer of the language itself but had created a foundation in combinatory logic and functional programming principles that the developers could build from. The latest formal specification of Haskell was made in July 2010, although several small revisions have been made to the language since then.

### Attributes & Why It Was Chosen

Haskell contains many trademarks that are common among other functional languages. For example, Haskell is a strong, statically typed language that uses type inference. Values are type-checked at compile time, balancing concise code with safety and mostly avoiding the need to declare their types explicitly. Another key feature of Haskell is its immutability. All data values are immutable by default, meaning that it cannot be changed once it is created. This enables the creation of pure functions, which leave its original values untouched and cannot produce side effects such as mutation. What we consider to be variables are referred to as "values" in Haskell, since referring to them as the former contradicts their principle of immutability. An easy way to work around Haskell's immutability is to create an assignment to a second variable/value with the same name (also known as "shadowing" the old value). In an imperative program, this action would create a new variable that overwrites the old one rather than writing a new value to the same location.
Learning Haskell comes with a steep learning curve, and the most difficult hurdle to jump over is the transition between imperative and functional programming. For those accustomed to imperative logic, Haskell is not as initially readable or writable as your typical imperative language. However, its purity and expressiveness can help aid its readability, and its intuitiveness can easily be understood with some practice.

## Grammar & Syntax of Haskell

### Grammar

For its syntactic structure, Haskell uses a context-free structure using either an LL or LR parser. It generally uses a parser combinator, which combines smaller parsers into larger ones using recursive descent parsing. This would primarily be useful for small to medium-sized programs, whereas larger and more complex programs tend to use parser generators. These take a formal grammar definition such as BNF and generate Haskell code for the parser. The primary Haskell compiler (GHC) uses Happy as its parser generator, which is equivalent to the Yacc parser generator designed for C.
On the other hand, Haskell's lexical structure uses a context-sensitive grammar that relies heavily on indentation, meaning that the parsing of a token depends on the context of its preceding whitespace. In some cases, this leading whitespace can be translated into an explicit token, which allows the parser to treat it as a context-free language.

### Syntax

When considering the most fundamental components of imperative languages like Java or C++, the one that arguably differs the most from functional languages is their methods of implementing loops. Functional languages such as Haskell do not have imperative loop structures such as `for`, `while`, `do-while`, `until`, etc. These structures rely on mutable variables and side effects—neither of which are supported in functional languages. The most common method of simulating a loop structure in Haskell is to use a recursive function that calls itself at the end of each iteration.
Below is a simple but comprehensive program that displays the powers of two from 1 to 10 using a simple loop structure in Java:

```
public class Power
{
    public static void main(String[] args)
    {
        // declare and initialize variables to be used in loop
        final int MAX_NUM = 10;
        int result = 1;
        
        // display powers of two from 1 to 10 using a FOR loop
        for (int i = 1; i <= MAX_NUM; i++)
        {
            result = 2 * result;
            System.out.println("2 ^ " + i + " = \t" + result);
        }
        
        // display final result of computation
        System.out.println("\nFinal result: " + result);
        
    }
}
```

In this program, the variable `result` starts at 1 (2^0) and is repeatedly incremented by a factor of two and displayed using a simple `for` loop. The loop will continue until the number of iterations passes `MAX_NUM`, which is a constant integer set equal to 10. Once the loop is completed, the final computation (2^10, in this case) will be displayed. Now, let's see the same program in Haskell and review the syntax of each step:

```
main :: IO()
main = do
    -- initialize variables used in the loop (:: Int not necessary)
    let maxNum :: Int = 10
    
    -- calculate powers of two and display final result
    funcReturn <- calculatePower maxNum
    putStrLn $ "\nFinal result: " ++ show funcReturn

-- function (loop) that displays powers of two up to maxNum (10)
calculatePower :: Int -> IO Int
calculatePower maxNum = go 1 1       -- start state: i = 1, result = 1
  where
    go :: Int -> Int -> IO Int
    go i result
      | i > maxNum = return result   -- if calculations done, exit go function
      | otherwise = do
          let result' = result * 2   -- resultCurrent = result * 2
          putStrLn $ "2 ^ " ++ show i ++ " = \t" ++ show result'
          go (i + 1) result'         -- recursively continue the loop
```

The first step of this program is to initialize the constant `maxNum` using the `let` keyword. In this program, `maxNum` is declared an `Int` and given the value of 10. Note the naming convention of this constant. In Haskell, defining any variable in all caps would cause a runtime error, since the names of data constructors are required to begin with a capital letter. The next step is to enter a loop that will calculate the powers of two from 1 to 10. Loops are generally housed in their own functions, and the easiest and most conventional way to design loops in functional languages is to use recursion.
The first line of the `calculatePower` function is the function header, which contains one `Int` argument and an `IO Int` return value. This is followed by the function definition, which uses a helper function called "`go`" that contains the loop logic. The `go` function also returns an IO Int but has two arguments: the values of `i` and `result`, both of which start at 1. The loop condition is checked using an if-else statement (denoted by a vertical bar for each branch). The code for each iteration of the loop is executed within the `otherwise = do` portion of the go function after confirming that the loop condition is false.
Within this block, the modified value of `result` is stored in a new variable that functions as the current result (called `result'`), since variables in Haskell are immutable. This method of changing a variable's value can only be done in a loop structure such as the one used here. The value is then displayed to the terminal using `putStrLn` as the `println` equivalent and the `show` keyword for displaying values. Once the value is changed and displayed to the terminal, the `go` function is called recursively with the arguments `(i+1)` and `result'` to move to the next iteration of the loop with the updated result. This loop will continue to execute via these recursive calls until the condition `i > maxNum` is true. At this point, the recursive loop is done and the final result will be passed back to the main function as an `IO Int`.
An `IO` value of any type needs to be converted to its pure form before it can be used. In this program, the `IO Int` value returned by `calculatePower` cannot be displayed directly, so Haskell uses the `<-` symbol to bind the resulting integer to a new variable `funcReturn`, making `funcReturn` a pure `Int` value. In the case of an `IO()` return (which is loosely comparable to a `void` return in C++ or Java), the assignment operator (`=`) can be used. Finally, the final result of the expression (in this case, 2^10) would be displayed using `putStrLn` and `show funcReturn`.

## Project Design
As previously stated, this project will be designed to translate straightforward code from a high-level language into machine language, mimicking the function of a compiler or interpreter. The Haskell program will read code from a file written in an undetermined imperative language (will use C++ as an example in this section), and it will extract each line of code and determine the lexemes in each line. From there, the code will begin the complex translation process, of which the result assembly language will be written into a `.s` file. The newly translated assembly code will be read from the `.s` file, and the machine code translation will begin. Once done, the machine code will be written into a second file—this time with a `.txt` extension—which will contain the final translation of the imperative program.
Here is a basic layout of the program that will be implemented in Haskell:
* Define all lexemes in C++ programming language
  * Keywords, i.e.:
    * Data types
    * Print statements (`print`, `printf`)
    * Conditional keywords (`if`, `else`, `case`, `switch`, `goto`?)
    * Loop keywords (`for`, `while`, `do`, `until` if using Ruby)
  * Operators and reserved symbols
    * Computation, comparison, assignment
  * Delimiters
  * Specifiers (i.e. `%d`, `%h`)
  * Variable values
    * Integer and variations (`short`, `long`, `byte`, `signed` and `unsigned`)
    * Float, double
    * Char, string
    * Constants
    * Pointers
    * Arrays
* Read code from C++ file (use the MIPS architecture for assembly language)
  * Extract each line of code
  * Extract lexemes from each line
  * Determine the operation and assign the appropriate assembly operation, i.e.:
    * The imperative '`+`' operator corresponds to either an `add` or `addi` in MIPS
      * `add` used when adding two variables
      * `addi` used when adding one variable to numerical value
    * The imperative '`-`' operator also corresponds to `add` or `addi`, since there are no subtraction instructions in MIPS
  * Distinguish `.data` and `.text` sections of the assembly file during the translation process
  * Check for comments in C++ code (may replicate in the MIPS program)
* Write the resulting assembly code into a file called `assembly.s`
* Read code from assembly.s and translate into machine language
  * For each instruction:
    * Determine whether it is an R-type, I-type, or J-type instruction
    * Map operations to their opcode, since this is the first field of every machine language instruction regardless of its type
    * Determine the values of the remaining fields for each instruction (32 bits total for each type), possibly into an integer array based on the type
      * <u>R-type (6 fields)</u>:	`opcode`      `rs`      `rt`      `rd`      `shamt`      `func`
      * <u>I-type (4 fields)</u>:	`opcode`      `rs`      `rt`      `immediate`
      * <u>J-type (2 fields)</u>:	`opcode`      `jump target`
  * Write the final translation into a file called `bin_code.txt`

## Things to Consider (tentative list)
Here is an early list of things to keep in mind during the implementation process:
* For machine language: R-type, I-type, and J-type instructions
* For assembly language: data and code sections
* One-to-many correspondence between imperative and assembly operations
* Determine the type of parsing used in the given language
* Determine whether import/include statements translate
* Determine how to translate print statements with variables
* Determine how to handle the end of an imperative instruction
* Need to use labels in assembly language (used for branching)
  * Used for `if` statements, loops, functions
* Could separate fields in each machine language instruction written to bin_code.txt?
* Determine how to assign the appropriate general-purpose registers based on imperative code (could be difficult)
  * `$t0` through `$t9` are temporary registers, `$s0` through `$s7` are saved registers
  * `$v0` and `$v1` are used for function returns
  * `$a0` through `$a3` are used for passing arguments to functions
  * `$ra` used for the return address of `jal` (jump and link) instructions
  * Can use register `$zero` (or `$0`) to denote the value of 0
* Possible display concerns:
  * Determine how to handle float printing with numerical format specifiers (i.e. how to extract the number from `%.2f` in a `printf` statement)
  * Determine how to handle string literals and escape sequences (i.e. '`\n`' and '`\t`')
* Determine whether two `.hs` files should be used for the project
  * One file to translate from imperative to assembly language
  * One file to translate from assembly to machine language

## Sources
https://www.haskell.org/onlinereport/haskell2010/
https://en.wikipedia.org/wiki/Haskell
