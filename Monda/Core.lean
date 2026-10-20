/-!
# Core AST

Language-independent AST. A concrete language plugs in through `Lang`.

The AST is in ANF: arguments are atoms (`Term`), never nested expressions.
Arithmetic, field access, allocation and the like are ordinary functions
(primitives) called by name.

`Stmt` and `Block` are indexed by `n`, the number of enclosing loops.
`brk i` and `cont i` take `i : Fin n`, so jumping out of a loop that does not
exist is unrepresentable, and a function body has type `Block L 0`.
-/

namespace Monda

/-- Parameters of a concrete language: types of variables, function names
and literals. The core never inspects them. -/

structure Lang where
  Var : Type
  Fn  : Type
  Lit : Type

/-- An atom: the only thing allowed as an argument (ANF). -/


inductive Term (L : Lang)
  | var : L.Var → Term L
  | lit : L.Lit → Term L


mutual

  /-- A statement inside `n` enclosing loops. `call f args` is `assign` with
  the result discarded; multiple assignment is a function returning a pair,
  followed by `Fst`/`Snd`. -/

  inductive Stmt (L : Lang) : Nat → Type
    | assign {n : Nat} : L.Var → L.Fn → List (Term L) → Stmt L n     -- x := f(y₁..yₙ)
    | call   {n : Nat} : L.Fn → List (Term L) → Stmt L n             -- f(y₁..yₙ)
    | ite    {n : Nat} : L.Var → Block L n → Block L n → Stmt L n    -- if var then ... else ... end if
    | loop   {n : Nat} : Block L (n+1) → Stmt L n                    -- loop ... end loop
    | brk    {n : Nat} : Fin n → Stmt L n                            -- break
    | cont   {n : Nat} : Fin n → Stmt L n                            -- continue
    | ret    {n : Nat} : Term L → Stmt L n                           -- return


  /-- A sequence of statements; `nil` is the empty block (no-op).
  A custom list rather than `List (Stmt L n)`: the kernel rejects a nested
  `List` whose index changes (`loop` needs `n + 1` inside `n`). -/

  inductive Block (L : Lang) : Nat → Type
    | nil  {n : Nat} : Block L n
    | cons {n : Nat} : Stmt L n → Block L n → Block L n
end


/-- Builds a `Block` from a list, so blocks can be written with list syntax. -/

def Block.ofList {L : Lang} {n : Nat} : List (Stmt L n) → Block L n
  | []      => .nil
  | s :: ss => .cons s (ofList ss)

end Monda
