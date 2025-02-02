# Statement

Statements are fragments of code that get executed in sequence.
They, too, can be of different types:

## Expressions

Expressions can be evaluated: however, the result is ignored.

## Blocks

A block is a sequence of statements delimited by curly brackets (`{}`).

## If Statement

An if statement is composed of three parts: a condition, a "then" branch, and an optional "else" branch. This is the syntax:

    if (condition) <then branch> else <else branch>

## Switch Statements

A switch statement takes in a value, and compares it to a series of cases (integers).

## While Loops

A while loop executes the body statement as long as the condition is met. The syntax is the following:

    while (condition) <body>

## Do-While Loops

A do-while loop acts like a while loop, but it runs the body statement at least one time. This is the syntax:

    do <body> while (condition);

## For Loops

A for loop acts like a while loop, but along with a condition, it has an initialization statement and an "increment" statement that runs each time. This is the syntax:

    for (init; condition; increment) <body>

## Return Statement

Return statements must be in functions (and functions must have at least one return statement). It can take an argument, otherwise, the argument is automatically 0.

    return <argument>;

## Break Statement

Break and continue statements change the flow of execution: `break` ends the loop and skips to the end, while `continue` simply skips to the next iteration of the loop.

## Inline Assembly

B# supports writing inline assembly code as a statement. The syntax is using the keyword `asm` followed by a string literal.