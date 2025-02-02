# Templates

Templates are a feature of B#, comparable to C's structs. They can hold members (variables) and can have methods (functions exclusive to them).

## Definition

Templates are defined using the keyword `template`:

    template MyTemp<int x, int y>;

## Assignment

You can declare a variable as template and, optionally, assign a value to it:

    auto MyTemp myVar = <1, 3>

### Default values in templates

You can also set default values to a template's members:

    template MyTemp<int x = 5, int y = 8>;

If you don't, on an empty initialization it will not be initialized and will be memory noise.

    auto MyTemp myVar = <> // the same as writing <5, 8>

## Methods

Methods of a template are defined like this:

    fn int MyTemp::myFunction() {
        return self.x;
    }

When a method is defined, a default argument `self` is passed to it, which refers to the parent template.

## Accessing members / methods

You can access a the members of a variable defined with a template with the `.` operator:

    auto int myInt = myVar.x;

You can also access a member of a pointer that points to a template with the `->` operator:

    auto MyTemp* myPtr = &myVar;
    auto int myInt = myPtr->x;