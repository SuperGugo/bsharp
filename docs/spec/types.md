# Data Types

B# provides six primitive types. Templates and arrays are also considered types. B# does not provide any unsigned integer types.

## Primitive types

    char
Signed integer type. 1 byte wide.

    short
Signed integer type. 2 byte wide.

    int
Signed integer type. 4 byte wide.

    long
Signed integer type. 8 byte wide.

    float
Floating point type. 4 bytes wide.

    double
Double precision floating point type. 8 bytes wide.

### Pointers

Pointers are considered longs. As such, they are 8 bytes wide.

### "AmbiguousInteger"

B# treats integer literals as "AmbigousIntegers", which are 8 bytes wide and can be downcasted implicitly.

Note: this will be replaced when implicit casting is implemented, as literals will have a flag to allow downcasting.

## Arrays

Arrays are static (their size cannot be changed), and they are declaredlike this:

    auto int arr[5] = [1, 2, 3, 4, 5];

This creates an array of size 5 and populates it with the given values.

(TODO)
Arrays can also be populated with a single value:

    auto int arr[5] = [2]; // This is the same as [2, 2, 2, 2, 2]

## sizeof

`sizeof` can be used to obtain the size of a type:

    auto int z = sizeof float;   // z equals 4

or of an already defined variable:

    auto int x;
    auto int z = sizeof x;   // z equals 4

In reality, `sizeof` can return the size of any expression:

    auto int z = sizeof [1, 2, 4];  // z equals 24