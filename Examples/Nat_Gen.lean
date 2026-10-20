import Monda.Basic

namespace Monda

structure MondaNat where
  used    : UInt32
  data    : ByteArray
  bounds  : used.toNat ≤ data.size
  head    : used.toNat = 0 ∨ data.get! (used.toNat - 1) ≠ 0
  leading : ∀ i, used.toNat ≤ i → i < data.size → data.get! i = 0

def MondaNat.val (a : MondaNat) : Nat :=
  go a.used.toNat
where
  go : Nat → Nat
  | 0     => 0
  | k + 1 => go k + (a.data.get! k).toNat * 256 ^ k

namespace NatAddEx

def x (s : String) : MondaVar := MondaVar.named s
def t (k : Nat)    : MondaVar := MondaVar.auto k

def X (s : String) : Term MondaL := .var (MondaVar.named s)
def T (k : Nat)    : Term MondaL := .var (MondaVar.auto k)
def C (k : Nat)    : Term MondaL := .lit k

def asg {n : Nat} (v : MondaVar) (f : MondaFn) (args : List (Term MondaL)) : Stmt MondaL n :=
  .assign v f args

def cl {n : Nat} (f : MondaFn) (args : List (Term MondaL)) : Stmt MondaL n :=
  .assign MondaVar.blank f args

def natAdd : Block MondaL 0 := .ofList [
  -- if a.used > b.used: swap(a, b)
  asg (x "au") "GetUsed" [X "a"],
  asg (x "bu") "GetUsed" [X "b"],
  asg (t 0) "Gt" [X "au", X "bu"],
  .ite (t 0) (.ofList [
    asg (t 1) "Id" [X "a"],
    asg (x "a") "Id" [X "b"],
    asg (x "b") "Id" [T 1]
  ]) .nil,
  asg (x "au") "GetUsed" [X "a"],
  asg (x "bu") "GetUsed" [X "b"],

  -- newSz, overflow := RoundUp(b.used + 1, GRANULARITY)
  asg (x "newSz") "Add" [X "bu", C 1],
  asg (t 2) "RoundUp" [X "newSz", C 16],
  asg (x "newSz") "Fst" [T 2],
  asg (x "overflow") "Snd" [T 2],
  .ite (x "overflow") (.ofList [
    asg (t 15) "Null" [],
    .ret (T 15)
  ]) .nil,


  -- result := Heap.Alloc(OffsetOf(Nat, data) + newSz)
  asg (t 3) "OffsetOfData" [],
  asg (t 4) "Add" [T 3, X "newSz"],
  asg (x "result") "Alloc" [T 4],
  asg (t 16) "IsNull" [X "result"],
  .ite (t 16) (.ofList [.ret (X "result")]) .nil,
  asg (x "carry") "Id" [C 0],

  -- for i in 0 .. a.used
  asg (x "i") "Id" [C 0],
  .loop (.ofList [
    asg (t 5) "Lt" [X "i", X "au"],
    .ite (t 5) .nil (.ofList [.brk 0]),
    asg (t 6) "GetData" [X "a", X "i"],
    asg (t 7) "GetData" [X "b", X "i"],
    asg (t 8) "AddBytes" [T 6, T 7, X "carry"],
    asg (t 9) "Fst" [T 8],
    asg (x "carry") "Snd" [T 8],
    cl "SetData" [X "result", X "i", T 9],
    asg (x "i") "Add" [X "i", C 1]
  ]),

  -- for i in a.used .. b.used
  asg (x "i") "Id" [X "au"],
  .loop (.ofList [
    asg (t 10) "Lt" [X "i", X "bu"],
    .ite (t 10) .nil (.ofList [.brk 0]),
    asg (t 11) "GetData" [X "b", X "i"],
    asg (t 12) "AddByteCarry" [T 11, X "carry"],
    asg (t 13) "Fst" [T 12],
    asg (x "carry") "Snd" [T 12],
    cl "SetData" [X "result", X "i", T 13],
    asg (x "i") "Add" [X "i", C 1]
  ]),

  -- used := b.used; if carry: data[used] := 1; used += 1
  cl "SetUsed" [X "result", X "bu"],
  .ite (x "carry") (.ofList [
    cl "SetData" [X "result", X "bu", C 1],
    asg (t 14) "Add" [X "bu", C 1],
    cl "SetUsed" [X "result", T 14]
  ]) .nil,

  .ret (X "result")
]

end NatAddEx

end Monda
