import Monda.Core
import Monda.Machine
import Monda.Semantics

/-!
# Monda instance

Plugs Monda into the core AST: its variables, function names and literals.
-/

namespace Monda

/-- A Monda variable: `named` comes from the source code, `auto k` is a
temporary introduced by the translation to ANF, `blank` receives results
that are never read. -/

inductive MondaVar
  | auto  : Nat → MondaVar
  | blank : MondaVar
  | named : String → MondaVar
  deriving DecidableEq


/-- Functions and primitives are referred to by name. There is no
overloading, so each arity needs its own name. -/

abbrev MondaFn := String

/-- Literals are natural numbers; their interpretation is up to the semantics. -/

abbrev MondaLit := Nat


/-- Monda as a core language. An `abbrev`, so that Lean unfolds
`MondaL.Var` to `MondaVar` and the builder helpers typecheck. -/

abbrev MondaL : Lang := { Var := MondaVar, Fn := MondaFn, Lit := MondaLit }


/-- A runtime value of Monda. Numbers carry no width: `UInt8`, `UInt32`
and friends are distinguished by primitives and by typing, not by values.
Booleans are numbers, `0` is false. Pointers are abstract addresses,
`null` is separate. `unit` is the value of the `void`-like type of the
language; the machine has no such value, it is lowered to no registers. -/

inductive MondaVal
  | num  : Nat → MondaVal
  | ptr  : BlockUid → MondaVal
  | null : MondaVal
  | unit : MondaVal


/-- Truthiness in `ite`: a nonzero number. Anything else is false;
branching on a pointer or `unit` is ruled out by typing. -/

def MondaVal.truthy : MondaVal → Bool
  | .num n => n != 0
  | _      => false


/-- The heap is a set of blocks, as in CompCert: a pointer `ptr b` names
block `b`, and a block is raw bytes. Fields are offsets inside a block,
read and written by primitives. `none` means the block is not allocated. -/

abbrev MondaHeap := BlockUid → Option ByteArray


/-- Monda on the machine: what its literals and function names mean.
A literal still has no width, so `lit` is not defined yet. -/

def MondaSem : Sem MondaL where
  lit := sorry
  asm := sorry


/-- `v` is the null pointer. A procedure that returns a pointer
reports OOM by returning null. -/

def isNull (v : Register) : Prop := v = Register.ptr .null

end Monda
