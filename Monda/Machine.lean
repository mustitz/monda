/-!
# Machine

The abstract virtual machine shared by all languages: registers, memory and
instructions. A language maps onto it through `Sem` and may use only part of
it. Memory model, allocator and later GC live here.
-/

namespace Monda

/-- Identity of an allocated block. Only equality is meaningful. -/

structure BlockUid where
  uid : Nat
  deriving DecidableEq


/-- A machine pointer: null or the name of a block. -/

inductive Pointer
  | null  : Pointer
  | block : BlockUid → Pointer


/-- A register value: a machine word. `u1` is a flag (carry, comparison
result). Language values are not machine values: the frontend lowers them
to words and pointers. -/

inductive Register
  | u1  : Bool   → Register
  | u8  : UInt8  → Register
  | u16 : UInt16 → Register
  | u32 : UInt32 → Register
  | u64 : UInt64 → Register
  | i8  : UInt8  → Register
  | i16 : UInt16 → Register
  | i32 : UInt32 → Register
  | i64 : UInt64 → Register
  | ptr : Pointer → Register


/-- Truthiness in `ite`: a set flag. Anything else is false. -/

def Register.truthy : Register → Bool
  | .u1 b => b
  | _     => false


/-- Memory from the point of view of the machine: allocated blocks of raw
bytes. The machine does not know what a block holds, only that it is
allocated and how big it is. `none` means the block is not allocated. -/

abbrev Memory := BlockUid → Option ByteArray


/-- Width of a memory access. -/

inductive Width
  | b8 | b16 | b32 | b64


/-- Instructions of the machine. -/

inductive Instruction
  | load   : Width → Instruction   -- load w (block, offset) : value
  | store  : Width → Instruction   -- store w (block, offset, value)
  | alloc  : Instruction           -- alloc (size) : pointer
  | addCU8 : Instruction           -- addCU8 (a, b, carry) : (sum, carry)


/-- One step of the machine: an instruction takes `k` register values and
memory and gives `m` result registers and new memory. A relation, not a
function: `alloc` may return a pointer or null, both are valid runs. One
rule per instruction fixes its `k` and `m`, written next. -/

inductive Tick : {k m : Nat} →
    Instruction → Vector Register k → Memory → Vector Register m → Memory → Prop

end Monda
