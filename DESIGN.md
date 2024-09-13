# Design Notes

This document contains design notes about the past, present, and future of gig.
It is mostly just a place to keep my thoughts in order.

## Compiler flow

- lexing & parsing (glance)
- process imports
- type check
- lower to core language
- monomorphise
- closure conversion
- code gen

## Type Checking

The initial implementation did type checking and lowing to the core language in
a single pass over the AST. This enabled quick implementation since it means
we skip a whole intermediate structure (the typed AST).
Additionally we get to type check on the core language instead of the surface
language, which is a simpler task.

This has now been refactored into two passes due to the following motivations.

1. Produce a fully typed AST of the surface language that would be useful for
   third party libraries that want to analyse gleam code.
2. Enable implementing better type error messages, as with the previous method
   they would relate to the core language rather than the surface language.

In the type checker we pass around a Context object. This object contains
the current state of the type checker.
1. The name of the current module
2. The type variable environment
  - This is used to emulate mutable references. It is a map from Ref to TypeVar
  - Ref is just a wrapper for a unique integer, used to look up reference cells
  - TypeVar is either Bound with a type or Unbound with a type variable id
  - A Type is either a VariableType or a concrete type. A VariableType holds a
    Ref. We change the value corresponding to a Ref to simulate mutating a
    mutable reference.
3. The modules map
  - This lets you look up modules by name. Inside the modules is information
    regarding what types and functions are defined.
4. The type uid
  - This is used and incremented when creating a new type variable.

Type checking works by walking the AST and assigning types to each value or
expression. This might be either a concrete type for things like literals or a
type variable that needs to be inferred later.

When two types ought to be equal, we call the `unify` function on them. This
ensure the types are equal, or if one of the types is a variable it updates
the variable to be equal to the other type.
For example, the type of the arguments of a function call ought to be equal to
the type of the parameters; and the return type of every branch in a case
expression ought to be equal.

Once a function has been type checked it needs to be generalised. This means
finding any unbound variables in the function and turning them into type
parameters. This results in a Poly type, short for polymorphic, allowing the
function to be used with multiple different types.

Mutually recursive functions need to be type checked as a group. The call graph
helps us figoure out what these groups are.

## Dynamic

To support dynamic types we have a `Dynamic` custom type with variants for each
basic type in gleam.

```gleam
type Dynamic {
  Nil
  Bool(Bool)
  Int(Int)
  ...
}
```

For every type in your program, the compiler would then generate a function to
convert your type into Dynamic.
These functions can be called via a generic `dynamic.from` function.

```gleam
type Cat {
  Cat(name: String, age: Int)
}

// this will be auto generated
fn dynamic_from_Cat(cat: Cat) -> Dynamic {
  Tuple([String("Cat"), String(cat.name), Int(cat.age)])
}
```

We could consider adding the concept of atoms so we can encode the type name
more efficiently (if we encode the type name at all).

## Bit Arrays

Bit arrays are composed of a byte array and an offset and length in bits.

When created bit array are byte aligned. You can then create a window into them
that may or may not be byte aligned.

Appending bit arrays copies the data into a new byte alligned bit array.

```
Algorithm for copying unaligned data (untested):

for each i starting from offset/8 until (offset+length)/8
  read 2 bytes at position i
  bit shift them to byte-align
  write the first byte to the output
```
