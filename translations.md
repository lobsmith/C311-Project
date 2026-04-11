# Translation of Core Structures in Python
This file will contain a legend for translations from Python into MIPS Assembly language (QTSpim). This is meant to be used as a guide and will not be referenced in the Haskell program.

QTSpim Assembly code is divided into two sections:
* `.data`: contains variable values.
* `.text`: contains everything else.

For some instructions, code will need to be written into both sections. A template can be seen here:

```
    .data
; variable declarations go here

    .text
    .globl main
main:
    ; instructions go here
    jr $ra       ; command to exit the program (can also be done using 10 syscall)
```

Below are translations for the core structures intended to be tested in this project. Pseudo-instructions are used where needed for ease of coding.

## Simple print() Statement
### Printing a String Literal
<ins>Example</ins>: 
    `print("Hello World")`
<ins>`.data` translation</ins>:
    `strName: .asciiz "Hello World"`
<ins>`.text` translation</ins>:
    ```
    [add later]
    syscall
    ```

## Variable Declarations
### int & bool
<ins>Example</ins>: 
    `x = 5`
<ins>`.data` translation</ins>:
    `intName: .word 5`
<ins>`.text` translation</ins>:
    `li $t0, intName`
* For `bool` variables, treat as an integer and treat `True` as `1` and `False` as `0`.
### float
### String

## Two-Operand Arithmetic
### Add & Subtract (+, -)
Since there are no subtract instructions in MIPS, the instructions that correlate with addition must be used for subtraction as well.
#### Two variables
#### One variable and one immediate
#### Two immediates

### Multiply (*)
Use `sll` instruction?
### Divide
#### Division (/)
Use `srl` instruction?
#### Floor/Integer Division (//)
Use `div` instruction for `//` but not `/`, store result in `mflo`.
* Note: Rounds towards -inf, not zero.
### Modulo (%)
Use `div` instruction?
Store result in `mfhi`.
* Note: Rounds towards -inf, not zero.

### Exponentiation (**)
* Use looped multiplication in Haskell for this.

## Conditional Statements
Use conditional branch (jump) to a label.
### If-Else Statements
### Case Statements

## Loops
Use conditional branch (jump) to a label.
### For Loop
### While Loop

## Functions
Use unconditional branch to a label.
### Function Header
### Function Return
