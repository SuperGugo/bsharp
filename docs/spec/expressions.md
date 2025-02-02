# Expressions

Variables (or, to be precise, references to variables) are a type of expressions. 

Everything that returns a value is an expression. Here are all types of expressions in B#:

## Literals

A literal can be of four types: an integer, a float, an array or a template initialization.

    4       // an integer literal
    0xA6    // another integer literal (hexadecimal)
    0b101   // another integer literal (binary)
    'a'     // another integer literal (char)
    0.6     // a float literal
    [1, 2]  // an array literal, composed of integer literals
    "hello" // another array literal, composed of integer literals
    <1, 3>  // a template init literal
    
## Assignment

An assignment is, in fact, an expression, and not a statement. It returns a value: the new value assigned to the variable.

Variables can be assigned values through the following operators:

    =
    +=
    -=
    /=
    %=
    *=
    &=
    ^=
    |=
    ~=
    >>=
    <<=
    ++ // this is equal to += 1
    -- // this is equal to -= 1
    !! // this performs a logical NOT on the variable

## Operation

B# provides the following operators:

    +  // addition
    -  // subtraction
    *  // multiplication
    /  // division
    %  // modulus
    &  // bitwise AND
    ^  // bitwise XOR
    |  // bitwise OR
    ~  // bitwise NOT
    >> // right bitshift
    << // left bitshift
    !  // logical NOT
    (TODO)
    && // logical AND
    || // logical OR
    ^^ // logical XOR

It also provides the following operators to compare the two operands:

    ==
    !=
    >
    <
    >=
    <=

## Ternary operator

B# implements my favourite feature of programming languages, the ternary operator.

    x ? a : b   // if x, then return a. otherwise, return b.

## Function call

You can call a function and always expect a return value, since void types are not real in B#. As such, every function call is an expression.

## Casting

Casting is done in a very simple way:

    int x

For operations, it is very much reccomended to not trust the order of operation and use brackets:

    int (x+y)