import Monda.Machine
import Monda.Core


/-!
# Semantics

Big-step semantics of the core AST, parameterised by the language.
-/

namespace Monda

/-- Binding of language `L` to the machine: what its literals are and
which of its function names are machine instructions. Everything else
is a user function. -/

structure Sem (L : Lang) where
  lit     : L.Lit → Register
  asm     : L.Fn → Option Instruction


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
def Locals (L : Lang) : Type := L.Var → Option Register


/-- `l.set x v` is `l` with `x` assigned to `v`. -/
def Locals.set (l : Locals L) (x : L.Var) (v : Register) : Locals L :=
  fun y => if y = x then some v else l y


/-- `l.setMany xs vs` assigns `vs[i]` to `xs[i]` for every `i`;
if a variable repeats, the last assignment wins. -/
def Locals.setMany {k : Nat} (l : Locals L)
    (xs : Vector L.Var k) (vs : Vector Register k) : Locals L :=
  (xs.zip vs).foldl (fun l p => l.set p.1 p.2) l


/-- Value of an atom: a variable is looked up in `l`, a literal is interpreted. -/
def evalTerm (S : Sem L) (l : Locals L) : Term L → Option Register
  | .var x => l x
  | .lit c => some (S.lit c)


/-- Evaluates atoms in `l`; `none` if some variable is not assigned. -/
def evalArgs (S : Sem L) (l : Locals L) {k : Nat} (args : Vector (Term L) k) :
    Option (Vector Register k) :=
  args.mapM (evalTerm S l)


mutual

  /-- A statement runs from initial globals and locals to final ones
  and finishes with an outcome. -/

  inductive Stmt.BigStep (S : Sem L) :
      {n : Nat} →
      Memory → Locals L → Stmt L n →
      Memory → Locals L → Outcome Register n →
      Prop where

    -- xs := f(args), f is a machine instruction
    | assign {n k m : Nat}
      {g₀ g₁ : Memory} {l₀ : Locals L}
      {xs : Vector L.Var m} {f : L.Fn} {args : Vector (Term L) k}
      {instr : Instruction}
      {vs : Vector Register k} {rs : Vector Register m} :
      evalArgs S l₀ args = some vs →
      S.asm f = some instr →
      Tick instr vs g₀ rs g₁ →
      Stmt.BigStep (n := n) S g₀ l₀ (.assign xs f args) g₁ (l₀.setMany xs rs) .normal

    -- if c then thn else els, c is true
    | iteTrue {n : Nat}
      {g₀ g₁ : Memory} {l₀ l₁ : Locals L}
      {c : L.Var} {thn els : Block L n}
      {v : Register} {o : Outcome Register n} :
      l₀ c = some v →
      Register.truthy v = true →
      Block.BigStep S g₀ l₀ thn g₁ l₁ o →
      Stmt.BigStep S g₀ l₀ (.ite c thn els) g₁ l₁ o

    -- if c then thn else els, c is false
    | iteFalse {n : Nat}
      {g₀ g₁ : Memory} {l₀ l₁ : Locals L}
      {c : L.Var} {thn els : Block L n}
      {v : Register} {o : Outcome Register n} :
      l₀ c = some v →
      Register.truthy v = false →
      Block.BigStep S g₀ l₀ els g₁ l₁ o →
      Stmt.BigStep S g₀ l₀ (.ite c thn els) g₁ l₁ o

    -- break i
    | brk {n : Nat}
      {g₀ : Memory} {l₀ : Locals L}
      {i : Fin n} :
      Stmt.BigStep S g₀ l₀ (.brk i) g₀ l₀ (.brk i)

    -- continue i
    | cont {n : Nat}
      {g₀ : Memory} {l₀ : Locals L}
      {i : Fin n} :
      Stmt.BigStep S g₀ l₀ (.cont i) g₀ l₀ (.cont i)

    -- return t
    | ret {n : Nat}
      {g₀ : Memory} {l₀ : Locals L}
      {t : Term L}
      {v : Register} :
      evalTerm S l₀ t = some v →
      Stmt.BigStep (n := n) S g₀ l₀ (.ret t) g₀ l₀ (.ret v)

    -- loop body, body says leave
    | loopExit {n : Nat}
      {g₀ g₁ : Memory} {l₀ l₁ : Locals L}
      {body : Block L (n+1)}
      {o₀ : Outcome Register (n+1)} {o₁ : Outcome Register n} :
      o₀.exitLoop = some o₁ →
      Block.BigStep S g₀ l₀ body g₁ l₁ o₀ →
      Stmt.BigStep S g₀ l₀ (.loop body) g₁ l₁ o₁

    -- loop body, body asks for another iteration
    | loopAgain {n : Nat}
      {g₀ g₁ g₂ : Memory} {l₀ l₁ l₂ : Locals L}
      {body : Block L (n+1)}
      {o₀ : Outcome Register (n+1)} {o₁ : Outcome Register n} :
      o₀.exitLoop = none →
      Block.BigStep S g₀ l₀ body g₁ l₁ o₀ →
      Stmt.BigStep S g₁ l₁ (.loop body) g₂ l₂ o₁ →
      Stmt.BigStep S g₀ l₀ (.loop body) g₂ l₂ o₁


  /-- A block runs from initial globals and locals to final ones
  and finishes with an outcome. -/

  inductive Block.BigStep (S : Sem L) :
      {n : Nat} →
      Memory → Locals L → Block L n →
      Memory → Locals L → Outcome Register n →
      Prop where

    | nil {n : Nat} {g : Memory} {l : Locals L} :
      Block.BigStep (n := n) S g l .nil g l .normal

    | consNormal {n : Nat} {g₀ g₁ g₂ : Memory} {l₀ l₁ l₂ : Locals L}
      {s : Stmt L n} {b : Block L n} {o : Outcome Register n} :
      Stmt.BigStep S g₀ l₀ s g₁ l₁ .normal →
      Block.BigStep S g₁ l₁ b g₂ l₂ o →
      Block.BigStep S g₀ l₀ (.cons s b) g₂ l₂ o

    | consAbrupt {n : Nat} {g₀ g₁ : Memory} {l₀ l₁ : Locals L}
      {s : Stmt L n} {b : Block L n} {o : Outcome Register n} :
      o ≠ .normal →
      Stmt.BigStep S g₀ l₀ s g₁ l₁ o →
      Block.BigStep S g₀ l₀ (.cons s b) g₁ l₁ o

end

end Monda
