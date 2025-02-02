# Functions

In B#, functions are defined using the `fn` keyword. They have a return type, parameters and a body that can be executed. For example:

    fn int add(int a, int b) return a+b;

## Default parameters

Parameters can have default values: they must be the last ones, because they are optional. They are used this way:

    fn int add(int a, int b=4) return a+b;

## External functions

If you declare a function with `extrn fn`, the function will be defined at linking time.

    extrn fn int add(int a, int b);