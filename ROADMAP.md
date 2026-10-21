# Monda: roadmap

Design rationale is in [DESIGN.md](DESIGN.md).

## State

- Lake project with two libraries: `Monda` (core + Monda instance) and `Examples`
- Core AST (`Lang`, `Term`, `Stmt`, `Block`) compiles; `assign` holds
  destinations and arguments as `Vector`s
- One concrete machine (`Machine`): `Register` (machine words and pointers),
  `Memory` (blocks of bytes), `Instruction` (`load`, `store`, `alloc`,
  `addCU8`), `Tick` indexed by the number of arguments and results.
  Rules of `Tick` are not written yet
- Big-step semantics over a binding `Sem L` compiles
- Monda instance: `MondaVar`, `MondaL`, `MondaVal`, `MondaHeap`;
  `MondaSem` has `lit` and `asm` as `sorry`
- First example: `Nat_Gen` with `MondaNat`, its abstraction function `val`,
  `natAdd` in core AST and its goal `natAdd_spec`; `natAdd` still uses the
  old instruction names (`GetUsed`, `Fst`, ...) that have no rules.
  `Nat_Proof` holds `natAdd_correct` as `sorry`

## Next steps

- [x] `Outcome`: `normal`, `brk (Fin n)`, `cont (Fin n)`, `ret V`
- [x] Big-step semantics as a `mutual` inductive relation for `Stmt` and `Block`
- [x] Goal first: `natAdd_spec` in `Nat_Gen`, `natAdd_correct` in `Nat_Proof`
- [x] One concrete machine instead of a parameter; arity through the indices
      of `Tick`; `assign` with several destinations
- [ ] Machine, verified on its own:
  - [ ] `Tick` rules: `addCU8`, `store`, `load`, `alloc`
  - [ ] more instructions as `natAdd` needs them (comparisons, `Add`,
        `RoundUp` with an overflow flag)
  - [ ] lemmas: load after store, store does not touch other blocks,
        `alloc` gives a fresh block or null, determinism except `alloc`
- [ ] `MondaSem`: a literal needs a width (a pair of width and value in the
      Monda instance), `asm` maps names to instructions
- [ ] Calls of user functions: table `P` of bodies, a second rule for
      `.assign` when `S.asm f = none`
- [ ] `natAdd` rewritten for the real instructions (`addCU8`, ...)
- [ ] Generated getters and setters for `MondaNat` fields, their lemmas and proofs
- [ ] `MondaHeap` as the language-level heap, `IsNat` from the layout, the
      invariants of `MondaNat` and `val`
- [ ] Typing as a separate predicate (arity of calls included)
- [ ] WP or Hoare rules with progress, their soundness, proof of `natAdd_correct`

## Open questions

- Safety: a stuck run has no derivation, so `natAdd_spec` holds for it
  vacuously. Progress has to be part of WP
- `alloc` needs a free block: finite support of `Memory` or a counter
- `Add(bu, 1)` and `Add(OffsetOfData, newSz)` in `natAdd` can overflow;
  overflow of representation is a heap error, but `used` is still `UInt32`
- The tail `data[used..newSz)` is not zeroed; the code relies on `alloc`
- Idea: a typed `AllocNat(newSz)` returning an object with
  `data.size = newSz` and zeros, closing both points above
- Core namespace is `Monda`, although the core is generic:
  rename to `Monda.Core` or split into a separate library?
- Partial or total correctness
- Syntax for binding an operation to its model (`implementation ℕ.add in Nat`)

## Deferred

- Reference counting
- `defer`: candidate core construct `block body finally cleanup`;
  cleanup cannot fail, arguments are evaluated at exit (as in Zig)
- Resource budgets on top: heap (bytes, allocations), stack (call depth),
  iterations; within budgets `isNull r ∨` disappears from specs
- `genAuto` as `StateM Nat` for building AST
- Move AST builder helpers (`x`, `t`, `X`, ...) to a module once there is a second example
- Verification of the code generator
- Non-tree types (doubly linked lists)
- Mathlib dependency
- Sanity theorems for the semantics:
  - determinism, given deterministic instructions
  - frame: a statement does not change locals it never assigns
  - `loop []` has no derivation
  - `block body = loop (body ++ [brk 0])`, needs lifting `brk`/`cont` by one
- Block attributes (region heap/static/stack, access rights, owner thread):
  `Memory := BlockUid → Option Block`; for now every pointer is heap
