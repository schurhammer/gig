# gig

Gig is a gleam compiler written in gleam. Made with fairy dust and duct tape.

## How to use

`gleam run -m gig example.gleam [--release] [--gc] [--compiler=clang]`

This will compile the file to `example.c` and then use a c compiler to create a binary named `example`.

Also recommended to increase your stack size `ulimit -s unlimited`, otherwise stack overflows are likely (segfault).

Optional flags:
- release: enable optimisation
- gc: enable garbage collection (note: the current gc is slow af)
- compiler: the name/path of the c compiler

Dependencies:
- clang/gcc is needed to compile to binary (clang recommended)
- Boehm GC library needs to be available for --gc

## Feature / Todo List

### Basics

- [x] Modules (note: modules are resolved relative to the root file)
- [ ] Unqualified imports
- [x] Bools
- [x] Ints
- [x] Floats
- [ ] Number formats
- [x] Strings
- [x] Lists
- [x] Equality
- [x] Assignments
- [x] Discard patterns
- [x] Type inference
- [x] Type annotations
- [ ] Type imports
- [ ] Type aliases
- [x] Blocks
- [ ] Constants
- [x] Memory Management (GC/RC)

### Functions

- [x] Functions
- [x] Higher order functions
- [x] Anonymous functions
- [ ] Function captures
- [x] Generic functions
- [x] Pipelines
- [x] Labelled arguments
- [ ] Documentation comments
- [ ] Deprecations

### Flow control

- [x] Case expressions
- [x] Variable patterns
- [x] Constructor patterns
- [ ] String patterns
- [x] List patterns
- [x] Recursion
- [ ] Tail calls (note: the c compiler may do this for us)
- [x] Multiple subjects
- [x] Alternative patterns
- [x] Pattern aliases
- [ ] Guards
- [ ] Exhaustiveness checking

### Other Data types

- [x] Tuples
- [x] Custom types
- [x] Records
- [x] Record accessors
- [ ] Record updates
- [x] Generic custom types
- [x] Results
- [ ] Bit arrays

### Standard library

- [ ] Standard library package
- [ ] List module
- [ ] Result module
- [ ] Dict module
- [ ] Option module

### Advanced features

- [ ] Opaque types
- [ ] Use
- [x] Todo
- [x] Panic
- [x] Let assert
- [ ] Externals

## Contributing

I am not accepting code contributions at this time, feel free to make issues or discussions though.
