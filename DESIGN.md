# Monda: design

Decisions and their rationale. Current state and plans are in
[ROADMAP.md](ROADMAP.md). Implementation details live in module docstrings.

## Language

- Pascal/Ada-like syntax, no undefined behaviour (wrapping or checked arithmetic)
- Contracts (`requires`, `ensures`, `invariant`) are written in the code
- No proofs in `.monda` files; proofs are written separately, in the prover's language
- `function` is total and pure and can be translated to the prover;
  `procedure` may have effects and may not terminate
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
- For refinement of pure mathematical types the only runtime failures are
  resource exhaustion: heap (bytes, allocations) and stack (call depth).
  A result too large to represent is a heap error too: that much cannot be
  allocated. Sizes are limited by `MAX_UINT64`
- Budgets: correctness is proved under the assumption that the heap and
  stack budgets suffice, plus a practical estimate. Within budgets `null`
  is impossible; until budgets exist, specs allow it

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

1. Generic core: AST, control-flow semantics, WP/Hoare, separation logic
   over an abstract resource algebra
2. Machine: registers, memory, instructions; shared by all languages
3. Binding `Sem L M` of a language to a machine: literals, instruction names
4. Frontend: translation of a concrete language into the core

The memory model belongs to the machine, which is a parameter of the semantics.
AST and semantics are defined separately.

In code this is enforced by imports: `Core` and `Machine` import nothing
and know nothing of each other; they meet in `Semantics`. Dependencies go
core, machine ← semantics ← Monda instance ← examples.

### Core AST

- ANF: arguments are atoms only (variable or constant), no nested expressions
- Arithmetic, memory access and `Alloc` are machine instructions, called
  like functions; field access goes through generated getters and setters
- `break`/`continue` outside a loop are unrepresentable
- No compound values: a pure operation with two results is two
  instructions (`ByteSum`, `ByteCarry`). Tuples only if an effectful
  function ever needs several results
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
- The semantics is parameterised by a machine `M` and a binding
  `S : Sem L M`. `Sem` holds only what belongs to the language: literals
  (`lit`) and which function names are machine instructions (`asm`)
- State is split: `Locals L M` belong to the core
  (`L.Var → Option M.Register`), `M.Memory` is opaque and changed only by
  instructions
- Atoms have no effects and do not read memory; global variables are
  accessed through instructions, as in Clight
- `x := f(args)` with `S.asm f = some i` runs `M.tick i`

### Machine

An abstract virtual machine shared by all languages, an analogue of LLVM:
a verified assembler. It knows nothing about the AST.

- `Register`, `Memory`, `Instruction`, `tick`, `truthy`
- Control flow is not the machine's: `ite`, `loop` and calls live in the
  core AST. The machine only says whether a register leads into `then`
  (`truthy`), like a branch instruction
- `tick` is a relation, not a function: `Alloc` may return a pointer or
  null, both are valid runs
- Wrapping and checked arithmetic are different instructions of one
  machine, not different semantics. Different machines for one program
  are different levels of detail: byte memory vs abstract memory for proofs
- Language values are not machine values: a structure, closure or `BigInt`
  is lowered by the frontend and related to the model by refinement
- The machine can be built and verified on its own: lemmas about
  instructions (load after store, `Alloc` gives a fresh block) are what
  program proofs use

Monda machine:

- Registers: `num` without width (`UInt32` and `UInt64` are distinguished
  by instructions and typing), `ptr`, `null` (separate, not `ptr 0`) and
  `unit` (result of instructions that return nothing). Booleans are
  numbers. `void` has no values, so it is a type only, never a register
- Bytes exist only in memory: load and store convert between bytes and
  `num` and check `< 256`
- Memory: numbered blocks of raw bytes, as in CompCert,
  `BlockUid → Option ByteArray`. A pointer names a block: no pointer
  arithmetic, only equality. Fields are offsets inside a block
- The allocator is ideal: proofs assume it does not fail within the
  budget. A real allocator that needs more is the allocator's problem;
  the same goes for the stack

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

### Operations

Refinement of a type says how to read `MondaNat` as `ℕ`, not that
`natAdd` is `+`. Each operation is bound to a model operation
(`implementation ℕ.add in Nat`, syntax not fixed): minimal declarations,
never a hand-written `ensures`. For `f : T₁…Tₙ → R` bound to
`F : M₁…Mₙ → M_R` the compiler generates the commuting square

    IsTᵢ g₀ pᵢ mᵢ → run of f returns r → IsR g₁ r (F m₁ … mₙ)

Composition is proved once in the core (simulation), not per program.

- `IsT g p m` (the object at `p` in memory `g` is a valid `T` with model
  value `m`) is generated from the layout, the type invariants and the
  abstraction function
- For now `g` is the whole memory; frame conditions and separation logic
  come with WP

## Examples

Files in `Examples/` stand for what the compiler would generate from
`.monda` sources (written by hand for now): `Nat_Gen.lean` comes from
`Nat.monda`. Type definitions with invariants, abstraction functions and
function bodies in core AST are all generated.

The generated file holds statements as `def …_spec : Prop`; proofs are
written by hand in a separate file (`Nat_Proof.lean`) as
`theorem … : …_spec`. Regenerating does not erase proofs, and a proof
cannot weaken its statement.

Field access is generated too: getters and setters for each field
(`MondaNat$used`, `MondaNat!used`) as ordinary functions over memory
instructions, with lemmas about them and their proofs. Nothing
type-specific is trusted.
