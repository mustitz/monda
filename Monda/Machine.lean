/-!
# Machine

Abstract virtual machine shared by all languages, an analogue of LLVM:
registers, memory and instructions. A language maps onto it through
`Sem`. Memory model, allocator and later GC live here.
-/

namespace Monda

/-- A machine: what a register holds, what memory is, the instruction set,
and what one `tick` of an instruction does: it takes register values and
memory and gives a result and new memory. `tick` is a relation, not a
function: `Alloc` may return a pointer or null, both are valid runs.
`truthy` says whether a register leads into `then` of a conditional. -/

structure Machine where
  Register    : Type
  Memory      : Type
  Instruction : Type
  tick        : Instruction → List Register → Memory → Register → Memory → Prop
  truthy      : Register → Bool

end Monda
