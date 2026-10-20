import Monda.Core

/-!
# Semantics

Big-step semantics of the core AST, parameterised by the language.
-/

namespace Monda

/-- Semantic parameters of a language: what values and global state are,
and what literals and primitives mean. The core never inspects them. -/

structure Sem (L : Lang) where
  Val     : Type
  Globals : Type
  lit     : L.Lit → Val
  truthy  : Val → Bool
  prim    : L.Fn → List Val → Globals → Val → Globals → Prop


/-- How a statement or block finished, inside `n` enclosing loops.
`V` is the type of values returned by `ret`. -/

inductive Outcome (V : Type) : Nat → Type
  | normal {n : Nat} : Outcome V n
  | brk    {n : Nat} : Fin n → Outcome V n
  | cont   {n : Nat} : Fin n → Outcome V n
  | ret    {n : Nat} : V → Outcome V n


/-- What a `loop` does after its body finished with outcome `o`:
`none` means run the body again, `some o'` means leave the loop with `o'`. -/

def Outcome.exitLoop {V : Type} {n : Nat} :
    Outcome V (n+1) → Option (Outcome V n)

  | .normal          => none
  | .cont ⟨0, _⟩     => none
  | .cont ⟨i + 1, _⟩ => some (.cont ⟨i, by omega⟩)
  | .brk  ⟨0, _⟩     => some .normal
  | .brk  ⟨i + 1, _⟩ => some (.brk ⟨i, by omega⟩)
  | .ret v           => some (.ret v)


variable {L : Lang} [DecidableEq L.Var]


/-- Local variables of a function: `none` means not assigned yet. -/
def Locals (S : Sem L) : Type := L.Var → Option S.Val


/-- `l.set x v` is `l` with `x` assigned to `v`. -/
def Locals.set {S : Sem L} (l : Locals S) (x : L.Var) (v : S.Val) : Locals S :=
  fun y => if y = x then some v else l y


/-- Value of an atom: a variable is looked up in `l`, a literal is interpreted. -/
def evalTerm {S : Sem L} (l : Locals S) : Term L → Option S.Val
  | .var x => l x
  | .lit c => some (S.lit c)


/-- Evaluates atoms in `l`; `none` if some variable is not assigned. -/
def evalArgs {S : Sem L} (l : Locals S) (args : List (Term L)) : Option (List S.Val) :=
  args.mapM (evalTerm l)


mutual

  /-- A statement runs from initial globals and locals to final ones
  and finishes with an outcome. -/

  inductive Stmt.BigStep (S : Sem L) :
      {n : Nat} →
      S.Globals → Locals S → Stmt L n →
      S.Globals → Locals S → Outcome S.Val n →
      Prop where

    -- x := f(args)
    | assign {n : Nat}
      {g₀ g₁ : S.Globals} {l₀ : Locals S}
      {x : L.Var} {f : L.Fn} {args : List (Term L)}
      {vs : List S.Val} {v : S.Val} :
      evalArgs l₀ args = some vs →
      S.prim f vs g₀ v g₁ →
      Stmt.BigStep (n := n) S g₀ l₀ (.assign x f args) g₁ (l₀.set x v) .normal

    -- if c then thn else els, c is true
    | iteTrue {n : Nat}
      {g₀ g₁ : S.Globals} {l₀ l₁ : Locals S}
      {c : L.Var} {thn els : Block L n}
      {v : S.Val} {o : Outcome S.Val n} :
      l₀ c = some v →
      S.truthy v = true →
      Block.BigStep S g₀ l₀ thn g₁ l₁ o →
      Stmt.BigStep S g₀ l₀ (.ite c thn els) g₁ l₁ o

    -- if c then thn else els, c is false
    | iteFalse {n : Nat}
      {g₀ g₁ : S.Globals} {l₀ l₁ : Locals S}
      {c : L.Var} {thn els : Block L n}
      {v : S.Val} {o : Outcome S.Val n} :
      l₀ c = some v →
      S.truthy v = false →
      Block.BigStep S g₀ l₀ els g₁ l₁ o →
      Stmt.BigStep S g₀ l₀ (.ite c thn els) g₁ l₁ o

    -- break i
    | brk {n : Nat}
      {g₀ : S.Globals} {l₀ : Locals S}
      {i : Fin n} :
      Stmt.BigStep S g₀ l₀ (.brk i) g₀ l₀ (.brk i)

    -- continue i
    | cont {n : Nat}
      {g₀ : S.Globals} {l₀ : Locals S}
      {i : Fin n} :
      Stmt.BigStep S g₀ l₀ (.cont i) g₀ l₀ (.cont i)

    -- return t
    | ret {n : Nat}
      {g₀ : S.Globals} {l₀ : Locals S}
      {t : Term L}
      {v : S.Val} :
      evalTerm l₀ t = some v →
      Stmt.BigStep (n := n) S g₀ l₀ (.ret t) g₀ l₀ (.ret v)

    -- loop body, body says leave
    | loopExit {n : Nat}
      {g₀ g₁ : S.Globals} {l₀ l₁ : Locals S}
      {body : Block L (n+1)}
      {o₀ : Outcome S.Val (n+1)} {o₁ : Outcome S.Val n} :
      o₀.exitLoop = some o₁ →
      Block.BigStep S g₀ l₀ body g₁ l₁ o₀ →
      Stmt.BigStep S g₀ l₀ (.loop body) g₁ l₁ o₁

    -- loop body, body asks for another iteration
    | loopAgain {n : Nat}
      {g₀ g₁ g₂ : S.Globals} {l₀ l₁ l₂ : Locals S}
      {body : Block L (n+1)}
      {o₀ : Outcome S.Val (n+1)} {o₁ : Outcome S.Val n} :
      o₀.exitLoop = none →
      Block.BigStep S g₀ l₀ body g₁ l₁ o₀ →
      Stmt.BigStep S g₁ l₁ (.loop body) g₂ l₂ o₁ →
      Stmt.BigStep S g₀ l₀ (.loop body) g₂ l₂ o₁


  /-- A block runs from initial globals and locals to final ones
  and finishes with an outcome. -/

  inductive Block.BigStep (S : Sem L) :
      {n : Nat} →
      S.Globals → Locals S → Block L n →
      S.Globals → Locals S → Outcome S.Val n →
      Prop where

    | nil {n : Nat} {g : S.Globals} {l : Locals S} :
      Block.BigStep (n := n) S g l .nil g l .normal

    | consNormal {n : Nat} {g₀ g₁ g₂ : S.Globals} {l₀ l₁ l₂ : Locals S}
      {s : Stmt L n} {b : Block L n} {o : Outcome S.Val n} :
      Stmt.BigStep S g₀ l₀ s g₁ l₁ .normal →
      Block.BigStep S g₁ l₁ b g₂ l₂ o →
      Block.BigStep S g₀ l₀ (.cons s b) g₂ l₂ o

    | consAbrupt {n : Nat} {g₀ g₁ : S.Globals} {l₀ l₁ : Locals S}
      {s : Stmt L n} {b : Block L n} {o : Outcome S.Val n} :
      o ≠ .normal →
      Stmt.BigStep S g₀ l₀ s g₁ l₁ o →
      Block.BigStep S g₀ l₀ (.cons s b) g₁ l₁ o

end

end Monda
