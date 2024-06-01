# gig

Gig is a gleam compiler written in gleam.

## How to use

`gleam run -m gig program.gleam`

This will compile the file to `program.c` and then use gcc to compile to a binary named `program`.

Note: at the moment `program.gleam` must be a standalone gleam file. Imports are not supported yet.

## Feature / Todo List

### Basics

- [ ] Modules
- [ ] Unqualified imports
- [x] Bools
- [x] Ints
- [ ] Floats
- [ ] Number formats
- [ ] Strings
- [ ] Lists
- [x] Equality
- [x] Assignments
- [x] Discard patterns
- [x] Type inference
- [ ] Type annotations
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
- [ ] List patterns
- [x] Recursion
- [ ] Tail calls
- [x] Multiple subjects
- [x] Alternative patterns
- [ ] Pattern aliases
- [ ] Guards
- [ ] Exhaustiveness checking

### Other Data types

- [ ] Tuples
- [x] Custom types
- [x] Records
- [x] Record accessors
- [ ] Record updates
- [x] Generic custom types
- [ ] Results
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
- [ ] Todo
- [ ] Panic
- [ ] Let assert
- [ ] Externals

## Contributing

I am not accepting contributions at this time, feel free to make issues or discussions though.
