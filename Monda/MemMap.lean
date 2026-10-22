/-!
# MemMap

Layout of one memory object: the object is a sequence of runs of cells,
each cell of one width (`Cell`). The layout carries no values, only where
cells are and how wide. Values and the invariants that tie them to the layout
are in `MemBlock`.
-/

namespace Monda

/-- Kind of a memory cell, given by its width. One cell holds one value.
Not the same as `Width` in `Machine`, which is the width of an access. -/

inductive Cell
  | b8 | b16 | b32 | b64
  deriving DecidableEq


/-- Bytes in a cell of width `w`. -/

def Cell.bytes : Cell → Nat
  | .b8  => 1
  | .b16 => 2
  | .b32 => 4
  | .b64 => 8


/-- Layout of one object: runs of cells, from offset 0 on. `cell n _ w rest`
is `n` consecutive cells of width `w` followed by `rest`. A run is never
empty. Values live elsewhere, see `MemBlock`. -/

inductive MemoryMap
  | nil  : MemoryMap
  | cell : (n : Nat) → 0 < n → Cell → MemoryMap → MemoryMap


/-- Size of the object in bytes: the layout covers all of it, so the size is
not stored. -/

def MemoryMap.size : MemoryMap → Nat
  | .nil             => 0
  | .cell n _ w rest => n * w.bytes + rest.size


/-- The cell covering byte `p`: its start offset and kind; `none` if `p` is
outside the object. For the layout `[4 × b8, 2 × b32]`:
  `cellAt 3 = (3, b8)`,
  `cellAt 4 = (4, b32)`,
  `cellAt 7 = (4, b32)`,
  `cellAt 9 = (8, b32)`,
  `cellAt 12 = none`. -/

def MemoryMap.cellAt (m : MemoryMap) (p : Nat) : Option (Nat × Cell) :=
  go m 0
where
  go : MemoryMap → Nat → Option (Nat × Cell)
    | .nil, _ => none
    | .cell n _ c rest, base =>
      if p < base + n * c.bytes then
        some (base + (p - base) / c.bytes * c.bytes, c)
      else
        go rest (base + n * c.bytes)


/-- Cells overlapping the byte range `[off, off + len)`, in order, as
(start offset, kind). Meant for `len > 0`. For the layout `[4 × b8, 2 × b32]`:
  `cellsIn 2 4 = [(2, b8), (3, b8), (4, b32)]`,
  `cellsIn 4 4 = [(4, b32)]`,
  `cellsIn 6 4 = [(4, b32), (8, b32)]`,
  `cellsIn 12 4 = []`. -/

def MemoryMap.cellsIn (m : MemoryMap) (off len : Nat) : List (Nat × Cell) :=
  go m 0
where
  go : MemoryMap → Nat → List (Nat × Cell)
    | .nil, _ => []
    | .cell n _ c rest, base =>
      (List.range n).filterMap (fun k =>
        let s := base + k * c.bytes
        if s < off + len ∧ off < s + c.bytes then some (s, c) else none)
      ++ go rest (base + n * c.bytes)


/-- Every cell starts at an offset divisible by its size. `base` is the
offset where the layout starts. -/

def MemoryMap.aligned : MemoryMap → Nat → Bool
  | .nil, _ => true
  | .cell n _ c rest, base =>
    base % c.bytes == 0 && rest.aligned (base + n * c.bytes)


/-- Neighbouring runs have different kinds: `prev` is the kind of the run
before. -/

def MemoryMap.mergedAfter (prev : Cell) : MemoryMap → Bool
  | .nil             => true
  | .cell _ _ c rest => prev != c && rest.mergedAfter c


/-- Normal form: neighbouring runs have different kinds, so one state of
the object has one layout. -/

def MemoryMap.merged : MemoryMap → Bool
  | .nil             => true
  | .cell _ _ c rest => rest.mergedAfter c


/-- `prepend n c m` puts `n` cells of kind `c` in front of `m`, merged with the
first run of `m` when it has the same kind. `prepend 0` adds nothing.
  `prepend 2 b8 []` is `[2 × b8]`,
  `prepend 2 b8 [3 × b8]` is `[5 × b8]`,
  `prepend 2 b8 [1 × b32]` is `[2 × b8, 1 × b32]`. -/

def MemoryMap.prepend (n : Nat) (c : Cell) (m : MemoryMap) : MemoryMap :=
  if h : 0 < n then
    match m with
    | .nil => .cell n h c .nil
    | .cell k hk c' rest =>
      if c = c' then .cell (n + k) (by omega) c rest
      else .cell n h c m
  else m


/-- A well-formed layout of an object that starts at offset `0`. -/

abbrev MemoryMap.WF (m : MemoryMap) : Prop :=
  m.aligned 0 = true ∧ m.merged = true


/-- Layout of the first `k` bytes. A cell cut by `k` falls apart into `b8`
bytes. For `[4 × b8, 2 × b32]`:
  `take 2 = [2 × b8]`,
  `take 6 = [6 × b8]`,
  `take 12 = [4 × b8, 2 × b32]`. -/

def MemoryMap.take : MemoryMap → Nat → MemoryMap
  | .nil, _ => .nil
  | .cell n _ c rest, k =>
    if n * c.bytes ≤ k then
      prepend n c (rest.take (k - n * c.bytes))
    else
      prepend (k / c.bytes) c (prepend (k % c.bytes) .b8 .nil)


/-- Layout of the bytes from offset `k` on, as a layout of its own. A cell cut
by `k` falls apart into `b8` bytes. For `[4 × b8, 2 × b32]`:
  `drop 2 = [2 × b8, 2 × b32]`,
  `drop 6 = [2 × b8, 1 × b32]`,
  `drop 8 = [1 × b32]`,
  `drop 12 = []`. -/

def MemoryMap.drop : MemoryMap → Nat → MemoryMap
  | .nil, _ => .nil
  | .cell n _ c rest, k =>
    if n * c.bytes ≤ k then
      rest.drop (k - n * c.bytes)
    else if k % c.bytes = 0 then
      prepend (n - k / c.bytes) c rest
    else
      prepend (c.bytes - k % c.bytes) .b8 (prepend (n - k / c.bytes - 1) c rest)


/-- Concatenation of two layouts, merging runs of the same kind at the seam.
  `concat [2 × b8] [3 × b8] = [5 × b8]`,
  `concat [4 × b8] [2 × b32]` is `[4 × b8, 2 × b32]`. -/

def MemoryMap.concat : MemoryMap → MemoryMap → MemoryMap
  | .nil, m             => m
  | .cell n _ c rest, m => prepend n c (rest.concat m)


/-- Layout after a cell of kind `c` is written at offset `off`: cells it
overlaps are gone, a cut cell falls apart into `b8` bytes. `none` if `off` is
not aligned to `c` or the cell does not fit. For `[4 × b8, 2 × b32]`:
  `set 8 b32 = [4 × b8, 2 × b32]` (same kind, the layout is unchanged),
  `set 0 b32 = [3 × b32]`,
  `set 5 b8  = [8 × b8, 1 × b32]` (the b32 at 4 is gone),
  `set 4 b16 = [4 × b8, 1 × b16, 2 × b8, 1 × b32]`,
  `set 2 b32 = none` (not aligned), `set 12 b8 = none` (outside). -/

def MemoryMap.set (m : MemoryMap) (off : Nat) (c : Cell) : Option MemoryMap :=
  if off % c.bytes = 0 ∧ off + c.bytes ≤ m.size then
    some ((m.take off).concat (prepend 1 c (m.drop (off + c.bytes))))
  else
    none


/-! ## Examples -/

def exMap : MemoryMap :=
  .cell 4 (by decide) .b8 (.cell 2 (by decide) .b32 .nil)

example : exMap.size = 12 := by rfl

example : exMap.cellAt 3  = some (3, .b8)  := by rfl
example : exMap.cellAt 4  = some (4, .b32) := by rfl
example : exMap.cellAt 7  = some (4, .b32) := by rfl
example : exMap.cellAt 9  = some (8, .b32) := by rfl
example : exMap.cellAt 12 = none           := by rfl

example : exMap.cellsIn 2 4  = [(2, .b8), (3, .b8), (4, .b32)] := by rfl
example : exMap.cellsIn 4 4  = [(4, .b32)]                     := by rfl
example : exMap.cellsIn 6 4  = [(4, .b32), (8, .b32)]          := by rfl
example : exMap.cellsIn 12 4 = []                              := by rfl


def badAlign : MemoryMap :=       -- b32 starts at offset 1
  .cell 1 (by decide) .b8 (.cell 1 (by decide) .b32 .nil)

def badMerge : MemoryMap :=       -- two neighbouring b8 runs
  .cell 2 (by decide) .b8 (.cell 2 (by decide) .b8 .nil)

example : exMap.WF        := by decide
example : ¬ badAlign.WF   := by decide
example : ¬ badMerge.WF   := by decide


example : MemoryMap.prepend 2 .b8 .nil = .cell 2 (by decide) .b8 .nil := by rfl

example : MemoryMap.prepend 2 .b8 (.cell 3 (by decide) .b8 .nil)
    = .cell 5 (by decide) .b8 .nil := by rfl

example : MemoryMap.prepend 2 .b8 (.cell 1 (by decide) .b32 .nil)
    = .cell 2 (by decide) .b8 (.cell 1 (by decide) .b32 .nil) := by rfl

example : MemoryMap.prepend 0 .b8 exMap = exMap := by rfl


example : exMap.take 2  = .cell 2 (by decide) .b8 .nil := by rfl
example : exMap.take 6  = .cell 6 (by decide) .b8 .nil := by rfl
example : exMap.take 12 = exMap                        := by rfl
example : exMap.take 20 = exMap                        := by rfl


example : exMap.drop 2 =
    .cell 2 (by decide) .b8 (.cell 2 (by decide) .b32 .nil) := by rfl
example : exMap.drop 6 =
    .cell 2 (by decide) .b8 (.cell 1 (by decide) .b32 .nil) := by rfl
example : exMap.drop 8  = .cell 1 (by decide) .b32 .nil := by rfl
example : exMap.drop 12 = .nil                          := by rfl


example : MemoryMap.concat (.cell 2 (by decide) .b8 .nil)
    (.cell 3 (by decide) .b8 .nil) = .cell 5 (by decide) .b8 .nil := by rfl


example : exMap.set 8 .b32 = some exMap := by rfl
example : exMap.set 0 .b8  = some exMap := by rfl
example : exMap.set 0 .b32 = some (.cell 3 (by decide) .b32 .nil) := by rfl

example : exMap.set 5 .b8 =
    some (.cell 8 (by decide) .b8 (.cell 1 (by decide) .b32 .nil)) := by rfl

example : exMap.set 4 .b16 =
    some (.cell 4 (by decide) .b8 (.cell 1 (by decide) .b16
      (.cell 2 (by decide) .b8 (.cell 1 (by decide) .b32 .nil)))) := by rfl

example : exMap.set 2 .b32 = none := by rfl
example : exMap.set 12 .b8 = none := by rfl


theorem MemoryMap.prepend_merged (n : Nat) (c : Cell) {m : MemoryMap} :
    m.merged = true → (prepend n c m).merged = true := by
  intro h
  unfold MemoryMap.prepend
  split
  . rename_i hn
    cases m
    . rfl
    . rename_i k hk c' rest
      dsimp only
      split
      . rename_i hc
        subst hc
        exact h
      . rename_i hc
        unfold MemoryMap.merged at h ⊢
        dsimp only at h ⊢
        unfold MemoryMap.mergedAfter
        simp [h, hc]
  . exact h

theorem MemoryMap.concat_merged (a : MemoryMap) {b : MemoryMap} :
    b.merged = true → (a.concat b).merged = true := by
  sorry


theorem MemoryMap.drop_merged {m : MemoryMap} (k : Nat) :
    m.merged = true → (m.drop k).merged = true := by
  sorry


theorem MemoryMap.set_wf {m m' : MemoryMap} {off : Nat} {c : Cell}
    (hwf : m.WF) (h : m.set off c = some m') : m'.WF := by
  unfold MemoryMap.set at h
  split at h
  . rename_i hc
    cases h
    refine ⟨?_, ?_⟩
    . sorry
    · exact MemoryMap.concat_merged _
        (MemoryMap.prepend_merged 1 c (MemoryMap.drop_merged _ hwf.2))
  · cases h

theorem MemoryMap.set_size {m m' : MemoryMap} {off : Nat} {c : Cell}
    (h : m.set off c = some m') : m'.size = m.size := by
  sorry


end Monda
