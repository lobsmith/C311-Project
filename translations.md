# Translation of Core Structures in Python
This file will contain a legend for translations from Python into MIPS Assembly language (QTSpim). This is meant to be used as a guide and will not be referenced in the Haskell program.

QTSpim Assembly code is divided into two sections using the following assembler directives:
* `.data`: contains variables and static data (i.e. string literals, immediates).
* `.text`: contains the code (executable instructions).

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
<ins>Example</ins>: `print("Hello World")`  
<ins>`.data` translation</ins>:
```
    strName: .asciiz "Hello World"
``` 
<ins>`.text` translation</ins>:
```
    la $a0, strName    ; load strName address into $a0
    li $v0, 4          ; load service number 4 into $v0 for printing string
    syscall            ; print strName
```

## Variable Declarations
### int & bool
<ins>Example</ins>: `x = 5`
<ins>`.data` translation</ins>:
```
    x: .word 5
```
<ins>`.text` translation</ins>:
```
    li $t0, x
```
Without using variable names, this operation can be translated using load immediate (`li, $t0, 5`) or add immediate (`addi $t0, $0, 5`) without writing to the `.data` section.
* For `bool` variables, treat as an integer and treat `True` as `1` and `False` as `0`.
### float
<ins>Example</ins>: `y = 3.14`  
<ins>`.data` translation</ins>:  
```
    y: .float 3.14
```  
<ins>`.text` translation</ins>:  
```
    l.s $f0, y    ; use floating point register ($f0-$f31)
``` 
### String
<ins>Example</ins>: `test = "Hello World"`  
<ins>`.data` translation</ins>:
```
    test: .asciiz "Hello World"
```
No `.text` translation needed.

## Two-Operand Arithmetic
### Add & Subtract (+, -)
Since there are no subtract instructions in MIPS, the instructions that correlate with addition must be used for subtraction as well.
#### Two variables
* Declare and initialize both variables (optional but is useful to track variable names in MIPS).
* Load both values into their own registers ($t0 & $t1 or $s0 & $s1 unless float).
* Use an `add` instruction to store the result in a third register.
#### One variable and one immediate
* Declare and initialize the variable (optional).
* Load the variable value into a register.
* Use an `addi` instruction to store the result in a second register.
#### Two immediates
Easiest solution: same protocol as one variable and one immediate
* Load the first value into a register.
* Add the second value and store the result using `addi`.

### Multiply (*)
Use `sll` instruction?
### Divide
#### Division (/)
Use `srl` instruction?
#### Floor/Integer Division (//)
Use `div` instruction for `//` but not `/`, store result in `mflo`.
* Note: Both `//` and the `div` instruction round towards -∞, not zero (for example, an actual quotient of -3.8 rounds down to -4).
### Modulo (%)
Use `div` instruction?
Store result in `mfhi`.
* Note: The `div` instruction rounds towards -∞, not zero.

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
