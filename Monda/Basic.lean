import Monda.Core

/-!
# Monda instance

Plugs Monda into the core AST: its variables, function names and literals.
-/

namespace Monda

/-- A Monda variable: `named` comes from the source code, `auto k` is a
temporary introduced by the translation to ANF. -/

inductive MondaVar
  | auto  : Nat → MondaVar
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

end Monda
