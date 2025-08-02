# gig

Gig is a gleam compiler written in gleam.

## How to use

```
git clone https://github.com/schurhammer/gig
cd gig
gleam run samples/hello_world.gleam
./samples/hello_world.exe
```

This will compile the file `samples/hello_world.gleam` to `samples/hello_world.c` and then use a c compiler to create a binary at `samples/hello_world`.

Optional flags:

- `--release`: enable optimisation
- `--gc`: enable garbage collection (otherwise no garbage collection)
- `--compiler=name`: the name/path of the c compiler

Dependencies:

- clang/gcc/tcc is needed to compile to binary (clang seems to work best for gc)
- Boehm GC library needs to be available for --gc

You may wish to increase your stack size `ulimit -s unlimited` to avoid stack overflows.

## Feature / Todo List

### Basics

- [x] Bools
- [x] Ints
- [x] Floats
- [x] Number formats (other than decimal)
- [x] Strings
- [x] Lists
- [x] Equality
- [x] Assignments
- [x] Discard patterns
- [x] Type inference
- [x] Type annotations
- [x] Modules (note: modules are resolved relative to the root file)
- [x] Dependencies
- [x] Unqualified imports
- [x] Type aliases
- [x] Blocks
- [x] Constants
- [x] Memory Management (GC/RC)

### Functions

- [x] Functions
- [x] Higher order functions
- [x] Anonymous functions
- [x] Function captures
- [x] Generic functions
- [x] Pipelines
- [x] Labelled arguments
- [ ] Documentation comments
- [ ] Deprecations

### Flow control

- [x] Case expressions
- [x] Variable patterns
- [x] Constructor patterns
- [x] String patterns
- [x] List patterns
- [x] Recursion
- [ ] Tail calls (note: the c compiler may do this for us)
- [x] Multiple subjects
- [x] Alternative patterns
- [x] Pattern aliases
- [x] Guards
- [ ] Exhaustiveness checking

### Other Data types

- [x] Tuples
- [x] Custom types
- [x] Records
- [x] Record accessors
- [x] Record updates
- [x] Generic custom types
- [x] Results
- [ ] Bit arrays (partial support)

### Standard library

There is limited support for standard library functions, see `stdlib/`.

### Advanced features

- [ ] Opaque types
- [x] Use
- [x] Todo
- [x] Panic
- [x] Let assert
- [x] Externals

## Contributing

I am not accepting code contributions at this time. Feel free to make issues, suggestions, or discussions though.
