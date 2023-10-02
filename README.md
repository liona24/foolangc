FooLangC - A simple Compiler
============================

This is a simple programming language named "FooLang" and its compiler.
The compiler is emitting x86-64 assembly code.

A hello world example:
```go
func print(int ptr, int size) {
    @syscall(1, 1, ptr, size, 0, 0, 0);
}

byte[6] prompt = "Hello\x0a";
print(&prompt, 6);

@exit(0);
```

Check out some examples in the `examples/` folder.

## Language

The language is statically typed and somewhat similar to C.
The following is possible:

```go
# single line comments

# numbers
int a = 1;      # signed 64 bit
byte b = \1;    # unsigned 8 bit

# pointers
int* c = &a;
*c = 123;

# arrays
int*[5] d = \0;
d[0] = c;
byte[5] hello = "Hello";

# structures
struct foo {
    int a;
    int b;
}
struct foo e = 0;
e.a = 1234;
e.b = 5678;

# function definitions
func bar(int arg0, struct foo arg1, struct foo* arg2) = int {
    return arg0 + arg1.a + arg2.b;
}
int res = bar(123, e, &e);

# loops and if conditions
int i = 0;
loop {
    i = i + 1;

    # logical negation using not
    # less than comparison
    if not (i < 10) {
        break;
    } elif 1234 == 1230 and 1 == 1 {
        @exit(1); # builtins prefixed with @
    } else {
        continue;
    }
}
```

The type system is very generous and will gladly insert implicit casts everywhere.
An implicit cast is always allowed if the size of the target type is greater or equal
to the size of the casted type.
The rest will be zero filled.
This means you can do the following to fill f.e. an array with zeros:
```cpp
byte[1234] a = \0;
```
Accessing single bytes of an integer works similarly:
```cpp
byte[8] a = 0x1122334455667788;
# a[0] == 0x88
# a[7] == 0x11
```

Explicit casts are not implemented currently :)

## Some Implementation Details

The compiler operates in 4 stages.

The first stage is the lexing / parsing of the user written code.
The lexer is built on top of the `regex` crate, which also is the only real external
dependency (see `src/foolang.rs`).
Parsing is done using an Earley Parser which is coded somewhat genericly,
however the tokens and symbols are currently hardcoded for the language
(see `src/parsing.rs` for the parser implementation and `src/foolang.rs` for the
grammar definition).

The resulting parse forest is then converted into a simpler AST (`see src/tree_simplification.rs`).
This tree is structured so that a single right-favored post-order-traversal will
yield all the nodes in the correct order as if the program was linearly executed.
To keep the tree structure a little bit less messy, the tree is allowed to nest,
bypassing the stricter requirements for the post-order-traversal.
There are few nodes making use of it, f.e. function and loop bodies etc.

You can render the AST (and the parse forest, too) using `graphviz` if you install the optional
dependencies.

After the tree is built, it is directly annotated with types in a single traversal.
This results in type information flowing from bottom to top.
Similarly to a SSA, each node defines one output and consumes all children as input.
Thus after the type annotation traversal, each node will have a output ready in a virtual register
along with the type of that output.
The whole type system is defined in `src/codegen/ir.rs`.
Type annotation is done in `src/tree_simplification.rs`.

After that the tree is translated into a linear IR, which is a little bit more,
well, linear.
This whole step is probably not really needed, but it does make the code generation
easier.
See `src/codegen/ir_translation.rs` for the details on the translation.

Finally the IR is translated into assembly code (`src/codegen/mod.rs`).
Each virtual register is assigned a real one using the helpers provided in `src/codegen/register_state.rs`.
Register allocation is done greedily.
Since the IR is somewhat SSA-ish, this is relatively straightforward.
Registers only have to be valid for the duration of a single IR operation.

There are a few complexities involved because the code generator takes care of
casting as well.
Ideally one should probaly refactor this bit, but who has the time for that.

The generated assembly is highly inefficient, but it works (hopefully more often that not).
Also note, that it uses a all-arguments-on-the-stack calling convention.
Theoretically this is probably not needed (even in the current implementation), but
since the register allocator is kept simple, this was the easiest solution to get correct.


### Error Handling

All compiler errors emitted are found in `src/error.rs`.
They use lexer information to render a pretty neat error message (most of the time at least TM).

The compiler will only continue until it encounters the first error.
Nobody has time for failure recovery.


## Contribution

Found a bug? Good for you. Feel free to fix it!
Also, there are several TODOs scattered across the codebase. Feel free to address them if you want :)
