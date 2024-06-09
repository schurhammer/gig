# gig

Gig is a gleam compiler written in gleam. Made with fairy dust and duct tape.

## How to use

`gleam run -m gig example.gleam [--nogc] [--release]`

This will compile the file to `example.c` and then use gcc to create a binary named `example`.

Optional flags:
- nogc: disable garbage collection
- release: enable optimisation

Dependencies:
- gcc is used to compile to binary
- Boehm GC library needs to be available (unless you use --nogc)

## Feature / Todo List

### Basics

- [x] Modules (note: modules are resolved relative to the root file)
- [ ] Unqualified imports
- [x] Bools
- [x] Ints
- [ ] Floats
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
- [ ] Tail calls
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
