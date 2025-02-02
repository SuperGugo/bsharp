# Preprocessing

Preprocessing directives are lines of code prefixed by a `#`.

B#'s preprocessor implements three directives:

## `include`

`include` can be used to include (duh) another file (usually a header file) to your code. The desired file is pasted in your code, replacing the directive.

## `define`

`define` can be used to define macros, using the following syntax:

    #define PI 3.14

## `undef`

Forgets a previously set macro.
