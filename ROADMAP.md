# Monda: roadmap

Design rationale is in [DESIGN.md](DESIGN.md).

## State

- Lake project with two libraries: `Monda` (core + Monda instance) and `Examples`
- Core AST (`Lang`, `Term`, `Stmt`, `Block`) compiles
- Abstract machine (`Machine`): registers, memory, instructions, `tick`, `truthy`
- Big-step semantics over a machine `M` and a binding `Sem L M` compiles
- Monda machine: registers (`MondaVal`) and memory (`MondaHeap`) defined,
  instruction set not yet
- First example: `Nat_Gen` with `MondaNat`, its abstraction function `val`,
  `natAdd` in core AST and its goal `natAdd_spec`; `Nat_Proof` holds
  `natAdd_correct` as `sorry`

## Next steps

- [x] `Outcome`: `normal`, `brk (Fin n)`, `cont (Fin n)`, `ret V`
- [x] Big-step semantics as a `mutual` inductive relation for `Stmt` and `Block`
- [x] `Machine` separate from `Sem`; `Sem L M` holds `lit` and `asm` only
- [x] Goal first: `natAdd_spec` in `Nat_Gen`, `natAdd_correct` in `Nat_Proof`
- [ ] Machine, independent of the AST and verified on its own:
  - [ ] move `MondaVal`, `BlockUid`, `MondaHeap`, `truthy` to `Machine.lean`
  - [ ] instruction set: what `natAdd` and the accessors need
  - [ ] `tick` as an inductive relation, one rule per instruction
  - [ ] lemmas: load after store, store does not touch other blocks,
        `Alloc` gives a fresh block or null, determinism except `Alloc`
- [ ] Calls of user functions: table `P` of bodies, a second rule for
      `.assign` when `S.asm f = none`
- [ ] `natAdd` without compound values: `ByteSum`/`ByteCarry`,
      `RoundUp`/`RoundUpOverflows` instead of `Fst`/`Snd`
- [ ] Generated getters and setters for `MondaNat` fields, their lemmas and proofs
- [ ] `IsNat` from the layout, the invariants of `MondaNat` and `val`
- [ ] Typing as a separate predicate
- [ ] WP or Hoare rules, their soundness, proof of `natAdd_correct`

## Open questions

- `Add(bu, 1)` and `Add(OffsetOfData, newSz)` in `natAdd` can overflow;
  overflow of representation is a heap error, but `used` is still `UInt32`
- The tail `data[used..newSz)` is not zeroed; the code relies on `Alloc`
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
  - determinism, given deterministic primitives
  - frame: a statement does not change locals it never assigns
  - `loop []` has no derivation
  - `block body = loop (body ++ [brk 0])`, needs lifting `brk`/`cont` by one
- Block attributes (region heap/static/stack, access rights, owner thread):
  `MondaHeap := BlockUid → Option Block`; for now every pointer is heap
