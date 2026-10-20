# Monda: design

Decisions and their rationale. Current state and plans are in
[ROADMAP.md](ROADMAP.md). Implementation details live in module docstrings.

## Language

- Pascal/Ada-like syntax, no undefined behaviour (wrapping or checked arithmetic)
- Contracts (`requires`, `ensures`, `invariant`) are written in the code
- No proofs in `.monda` files; proofs are written separately, in the prover's language
- Reference counting for strings and memory, as in Delphi
- `Ptr A` and `FieldPtr A B` (pointer to a field of `A`, with a safe `container_of`)
- Doubly linked circular lists must be expressible as ordinary code, not `unsafe`

## Math layer

- Inductive types, records, `Prop` fields
- Mathematical types may be borrowed from the prover's standard library
- Proof fields are erased at compile time (zero overhead)

## Monads and errors

- Errors are tied to monads: each monad carries its own error type
- `Heap` has a single error, OOM, so it is a Maybe monad
- `monad` syntax removes manual checks (`if ptr = Null ...`): `bind` inserts
  them, and codegen turns them into `if (!p) goto fail`
- The compiler stacks monads
- Only order-invariant stacks are allowed. Canonical form: all states as a
  product, all errors as a sum, state survives an error
  (`OptionT (StateM Heap)`, because that is how the runtime behaves)
- Rollback (transactions) only explicitly, by hand
- Postconditions hold only on the success path; guarantees like
  "`none` only if `Alloc` returned `none`" are not needed for now
- Contracts may call only pure functions
- Monads exist only in the source language: the frontend expands `bind`
  into explicit checks, the core AST and its semantics have no monads

## Verification

- Prover: Lean first
- Deep embedding: heap, code and semantics are formalised in Lean, and
  program correctness is proved against that semantics
- Shallow embedding rejected
- Simple facts are eventually discharged by a built-in SMT solver, hard ones go to the prover

### Generic core

The semantic core is independent of Monda, so that C, Pascal, Java and
others could map onto it. Monda is the only instance so far.

Layers:

1. Generic core: AST, execution semantics, WP/Hoare, separation logic over
   an abstract resource algebra
2. Language parameters: values, memory model, primitives, outcomes
3. Frontend: translation of a concrete language into the core

The memory model is a parameter, not part of the core.
AST and semantics are defined separately.

In code this is enforced by imports: the core imports nothing from Monda,
dependencies go core ← Monda instance ← examples.

### Core AST

- ANF: arguments are atoms only (variable or constant), no nested expressions
- Arithmetic, field access and `Alloc` are ordinary functions (primitives)
- `break`/`continue` outside a loop are unrepresentable
- Multiple assignment is sugar: a function returns a pair, then `Fst`/`Snd`
- No call statement: `f(args)` is `_ := f(args)`, fewer cases in every proof
- No goto: proofs get too hard. No Wasm-style `block`:
  `block body = loop (body ++ [brk 0])`
- Extrinsic typing: the AST is untyped, types are a separate predicate

### Semantics

- Big-step, as a relation: `Stmt.BigStep` and `Block.BigStep` (mutual)
- A non-terminating run has no derivation, so the semantics covers
  terminating runs only
- `Outcome` (`normal`, `brk`, `cont`, `ret`) is indexed by loop depth, like
  the AST: a function body can only finish with `normal` or `ret`
- Blocks pass any outcome except `normal` upwards; only `loop` consumes
  signals, through `Outcome.exitLoop`, so a loop needs just two rules
- No `fail` outcome: OOM is a null pointer returned by the function,
  monads may change this later
- Semantic parameters (`Sem`) are separate from syntax (`Lang`): one AST
  can have several semantics (wrapping vs checked arithmetic,
  concrete vs abstract memory)
- State is split: `Locals` belong to the core (a function
  `Var → Option Val`), `Globals` are opaque and changed only by primitives
- Atoms have no effects and do not read `Globals`; global variables are
  accessed through primitives (`GetGlobal`/`SetGlobal`), as in Clight
- Primitives are relations, not functions: `Alloc` may return a pointer
  or null, both are valid runs
- Truthiness of a value in `ite` is a semantic parameter (`truthy`)

### References

- Iris-Lean: HeapLang as a template for "AST + semantics + WP + SL"
- CSLib: general infrastructure for operational semantics (LTS, reduction systems)
- Loom / Velvet: shallow, but a reference for SMT + interactive proofs
- `mvcgen` (Std.Do): WP calculator design

## Refinement instead of isomorphism

Runtime types relate to mathematical ones by refinement, not isomorphism.
Example: `MondaNat ≃ ℕ` is impossible, because large numbers are not
representable and different capacities give different objects with the same
value.

`refinement MondaNat → ℕ via ℕ_from_Nat for n < 256^MAX_UINT32`
produces two obligations:

1. `∀ x : MondaNat, ℕ_from_Nat x < 256^MAX_UINT32`
2. `∀ n < 256^MAX_UINT32, ∃ x, ℕ_from_Nat x = n`

Injectivity is not required.

## Examples

Files in `Examples/` stand for what the compiler would generate from
`.monda` sources (written by hand for now): `Nat_Gen.lean` comes from
`Nat.monda`. Type definitions with invariants, abstraction functions and
function bodies in core AST are all generated.
