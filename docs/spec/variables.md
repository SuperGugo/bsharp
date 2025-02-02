# Variables

Variables are references to a memory area. They each have a type and a storage duration specifier.

## Storage Duration

When declaring a variable, the first thing you need to specify is the storage duration, which tells the compiler where the variable will be accessible from. This can be done through the use of three specifiers:

    auto

`auto` is the default specifier, <(TODO) and as such it can be omitted>. It makes the variable only accessible from the scope it was declared in. If the variable was declared outside any scope, it is a global variable, and can be used anywhere in the program.

    static

Static variables are considered global, even if declared inside a scope.

(TODO) Static variables, like `auto` variables, can only be used in the scope they were declared in, but their value persists between function calls.

    extrn

`extrn` variables are declared, but not defined. They are, in fact, only defined at linking time. They are most useful in header files. Actually, I don't see a case where you would use it outside a header file.

Note: Definition of a variable is the process in which the variable is assigned an address in memory.

## Declaration

With all these things considered, this is how a variable is declared:

    auto int x;

In order: a storage duration specifier, a data type (which, technically, can be omitted and defaults as int) and an identifier.

## Reference

Variables can be referenced in three ways:

- as values, by simply using the variable's identifier:

        x

- as references (a reference to the area of memory where the variable is stored)

        &x
    
    Note: this returns a pointer. If x was of type int, &x will be of type int*.

- as pointers (returns the value in the memory area pointed to by the variable)

        *x

    Note: this takes in a pointer. If x was of type int*, *x will be of type int.

In a broader sense, the subscript operator `[]` is also a variable reference: it is a pointer to an addition between the reference of the variable and the value inside the subscript operator.

The same concept also applies to the template member access `.` operator and the template member of pointer access `->` operator, respectively used to access a member of a template type variable and a pointer of a template.