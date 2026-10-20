# Monda: roadmap

Design rationale is in [DESIGN.md](DESIGN.md).

## State

- Lake project with two libraries: `Monda` (core + Monda instance) and `Examples`
- Core AST (`Lang`, `Term`, `Stmt`, `Block`) compiles
- First example: `Nat_Gen` with `MondaNat`, its abstraction function `val`
  and `natAdd` written in core AST
- Big-step semantics (`Monda/Semantics.lean`) compiles

## Next steps

- [x] `Outcome`: `normal`, `brk (Fin n)`, `cont (Fin n)`, `ret V`
- [x] Language parameters (`Sem`): `Val`, `Globals`, literals, `truthy`, primitives
- [x] Big-step semantics as a `mutual` inductive relation for `Stmt` and `Block`
- [ ] Typing as a separate predicate
- [ ] Calls of user functions: table of function bodies
- [ ] Monda instance `MondaSem : Sem MondaL`: values, memory, primitives of `natAdd`
- [ ] WP or Hoare rules, their soundness, first VC for `natAdd`

## Open questions

- `Add(bu, 1)` and `Add(OffsetOfData, newSz)` in `natAdd` can overflow
- The tail `data[used..newSz)` is not zeroed; the code relies on `Alloc`
- Idea: a typed `AllocNat(newSz)` returning an object with
  `data.size = newSz` and zeros, closing both points above
- Core namespace is `Monda`, although the core is generic:
  rename to `Monda.Core` or split into a separate library?
- Partial or total correctness

## Deferred

- Reference counting
- `defer`: candidate core construct `block body finally cleanup`;
  cleanup cannot fail, arguments are evaluated at exit (as in Zig)
- Resource budgets (memory, iterations) on top
- `GetField`/`SetField` instead of a primitive per field
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
