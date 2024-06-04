# gig

Gig is a gleam compiler written in gleam. Made with fairy dust and duct tape.

## How to use

`gleam run -m gig program.gleam [--nogc] [--release]`

This will compile the file to `program.c` and then use gcc to compile to a binary named `program`.

Note: at the moment `program.gleam` must be a standalone gleam file. Imports are not supported yet.

Optional flags:
- nogc: disable garbage collection
- release: enable optimisation

Dependencies:
- gcc is used to compile to binary
- Boehm GC library needs to be installed (or use --nogc)

## Feature / Todo List

### Basics

- [ ] Modules
- [ ] Unqualified imports
- [x] Bools
- [x] Ints
- [ ] Floats
- [ ] Number formats
- [ ] Strings
- [x] Lists
- [x] Equality
- [x] Assignments
- [x] Discard patterns
- [x] Type inference
- [x] Type annotations (note: only for function parameters)
- [ ] Type imports
- [ ] Type aliases
- [x] Blocks
- [ ] Constants
- [ ] Memory Management (GC/RC)

### Functions

- [x] Functions
- [x] Higher order functions
- [x] Anonymous functions
- [ ] Function captures
- [x] Generic functions
- [ ] Pipelines
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
- [ ] Pattern aliases
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
