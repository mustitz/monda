# Informal Introduction to Monda

## A Pure Functional Language with Granular Effect Management

Monda is a pure functional programming language that takes a radically different approach
to managing side effects compared to traditional functional languages like Haskell.
Let's begin by examining a simple "Hello, World!" program that showcases the language's
core philosophy:

```monda
function main
returns ret_code: Stdout Exit_Code
guarantee ret_code = 0
monadic Stdout
  msg: String := "Hello, Monda!\n"
  <- print msg
  0
```

This seemingly simple program reveals several groundbreaking design decisions that set
Monda apart from other functional languages.

## Pure Do-Notation Programming

The most striking feature is that Monda programs are written entirely in do-notation style.
Unlike Haskell, where you might mix pure expressions with monadic `do` blocks,
Monda embraces a uniform monadic syntax throughout.
The `monadic` keyword replaces Haskell's `do`,
creating a consistent computational context where all operations are explicitly sequenced.

Notice how all operations exist within the monadic framework:
pure bindings use `:=` (similar to Haskell's `let` or `where`),
monadic operations use the familiar `<-` operator from Haskell's do-notation,
and even return values (`0`) are automatically lifted into the monadic context.

## Granular Monad Hierarchies

Rather than Haskell's monolithic `IO` monad that encompasses all side effects,
Monda employs a sophisticated hierarchy of specialized monads.
Each monad represents a specific type of computational effect:
- **Identity**: The base monad for pure computations
- **ForeignCall**: Handles interactions with external code and system calls
- **Stdout**: Manages standard output operations specifically

This granular approach allows for precise effect tracking.
When you see `monadic Stdout` in the function signature,
you know immediately that this function will produce output to stdout—nothing more, nothing less.
The language automatically handles conversions from general to more specific effects (and effect grouping),
so programmers can work at the appropriate level of abstraction without manual lifting.

## Automatic Monadic Lifting

One of Monda's most ambitious goals is the automatic lifting of monadic operations throughout the monad stack.
The language runtime handles the complex task of lifting operations between different monadic layers,

## A New Paradigm

Monda represents a bold experiment in functional language design:
what happens when you make monadic programming not just possible, but unavoidable and automatic?
By forcing all computation into monadic contexts while providing sophisticated automation for effect management,
Monda aims to give programmers the power of precise effect tracking without the traditional complexity overhead.

The result is a language where purity and effects coexist naturally,
where the type system tells you exactly what computational effects a function may produce,
and where the runtime handles the intricate details of effect composition automatically.

## Function Declaration Syntax

Monda's function syntax draws inspiration from both Python and Haskell,
creating a clean and expressive syntax that emphasizes clarity and flexibility.

Every function has the following sections:
- `function` with name and arguments
- `returns` with return type specification
- `requires` (optional) for preconditions
- `guarantee` (optional) for postconditions

All these sections support two forms: short form
(usually for one element or separated by comma, though comma separation is not yet implemented)
and long form where each element occupies a separate line.
Foreign functions include a `linkage` specification,
while ordinary functions have a `monadic` section containing code written in do-notation style.

### Function Arguments

Function arguments are declared using the `function` keyword followed by the function name:

```monda
function print obj: T
```

Long form example:

```monda
function write
  fd: Int
  buf: Pointer
  count: Size
```

### Return Type Specification

Return types are specified using the `returns` keyword
and can be written in either short or long form for tuples.
The return specification includes both the monadic context and the actual return type:

```monda
function write
  fd: Int
  buf: Pointer
  count: Size
returns ForeignCall SSize
```

### Function Contracts

Monda provides powerful mechanisms for expressing function contracts through preconditions and postconditions:

**Preconditions** are specified with the `requires` keyword
and define constraints that must hold when the function is called.
These can include type constraints, value ranges, or relationships between parameters:

```monda
function print obj: T
requires Showable T
returns Stdout SSize
monadic Stdout
  write obj.show
```

**Postconditions** use the `guarantee` keyword to specify conditions
that the function promises to maintain upon return:

```monda
function main
returns ret_code: Stdout Exit_Code
guarantee ret_code = 0
monadic
  0
```

### Function Body

Functions have two types of bodies depending on their purpose.
Foreign functions that interface with external libraries use the `linkage` keyword to specify external bindings:

```monda
function write
  fd: Int
  buf: Pointer
  count: Size
returns ForeignCall SSize
linkage "libc", "write"
```

Ordinary functions use the `monadic` keyword to introduce executable code written in do-notation style,
as described in the previous sections.

## Monda Buffers

Buffers are Monda's fundamental abstraction for working with raw memory,
essential for file operations and system interoperability.
They provide a simple but efficient way to handle binary data and interact with external libraries:

```monda
record Buffer
  ptr: Pointer
  size: Size
```

This design stores both the memory location and explicit size,
eliminating the need to scan for terminators when determining length.
Buffers are optimized for performance-critical operations
like file I/O, network communication, and interfacing with C libraries.

An important architectural decision is that buffers in Monda cannot exist as standalone objects—they must always be part of another data structure.
This ensures proper memory management and prevents dangling pointer issues,
as the containing object is responsible for the buffer's lifecycle.

## Monda Strings

Monda's string implementation is inspired by Delphi's approach,
prioritizing performance over the typical immutable string designs found in most functional languages.
The design addresses a common pain point:
functional languages often suffer from slow string operations due to excessive copying and allocation overhead.

Strings layer reference counting on top of the buffer foundation:

```monda
record String
  ref_count: Int64
  buffer: Buffer
```

This buffer-based design provides several advantages over traditional functional language string implementations:

1. **Zero-copy sharing**: Multiple references to the same string share the underlying buffer until modification
2. **Efficient C interop**: Null-terminated strings work directly with C libraries without conversion
3. **Fast length queries**: Buffer size is stored, eliminating the need to scan for null terminators
4. **Optimized constants**: String literals are embedded directly in the binary with static reference counts
