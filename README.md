# Monda

A hobby experiment: a Pascal/Ada-like systems language with contracts,
where code and mathematics live in one world. Programs are verified in
Lean via a deep embedding into a language-independent semantic core.

Early stage, nothing is fixed.

## Build

```
nix-shell
lake build
```

Lean version is pinned in `lean-toolchain`.

## Layout

- `Monda/`: the library (generic core and the Monda instance)
- `Examples/`: tests, hand-written for now as if generated from `.monda` sources

## Docs

- [DESIGN.md](DESIGN.md): design decisions and their rationale
- [ROADMAP.md](ROADMAP.md): current state, next steps, open questions
