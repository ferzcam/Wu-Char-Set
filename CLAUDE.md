# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

Haskell implementation of Wu's characteristic set method for automated geometric theorem proving, targeting the IMO-AG-30 benchmark. Built with Stack (resolver `lts-18.21`) on top of konn's `computational-algebra` (pinned git extra-dep providing `halg-core` / `halg-polynomials`).

## Commands

Fernando runs build/test/exec commands himself — make edits and ask him to run them rather than invoking Stack directly.

- Build: `stack build`
- Run the full suite over `problems/dsl/` (30s per-problem timeout): `stack run Wu-Char-Set`
- Custom timeout / all: `stack run Wu-Char-Set -- --timeout 120 --all`
- Specific files: `stack run Wu-Char-Set -- problems/dsl/simson_line.dsl`
- Alternate directory: `stack run Wu-Char-Set -- --dir path/to/dsl`
- Tests: `stack test`

The first build compiles `computational-algebra` from git and takes several minutes; subsequent builds are incremental.

## Pipeline architecture

A DSL file drives a four-stage flow; to touch any stage productively you almost always need to read at least two adjacent modules.

1. **Parse** — `Util/Algebraizer.hs` (`parseProblem`). DSL reference lives in README.md; each line is a `GeoStep` (triangle, circumcenter, midpoint, foot, inter_ll/cl/cc, perp/para/cong/eqangle, `prove_*`, …). `algebraize` expands each step into `Hypothesis`/`Conclusion` values from `Util/Tokenizer.hs` over abstract coordinates `X "..."` (dependent) and `U "..."` (free). Result: `AlgResult { arHypotheses, arConclusion, arNumXVars }`.

2. **Algebraize to polynomials** — `Util/Tokenizer.hs` (`generatePolynomials`). Hypotheses + conclusion become `Polynomial' n = OrderedPolynomial Rational Grevlex n`. Variable ordering is critical for Wu: conclusion X-vars come first, then remaining X-vars, then U-vars. **U-values are substituted with the fixed numeric sequence `[0,1,4,9,16,…]`** in `generateVariables`/`monicPolys` (line ~55), so the solver proves one concrete instance rather than working symbolically — bad instances can spuriously fail. This is the primary tuning knob documented in README.md's TODO.

3. **Wu characteristic set** — `Polynomial/Wu.hs` (`charSet`). Triangularizes hypotheses by iterating pseudoremainder chains against the class variable. `analizeS` / `maxNpseudo` handle the multi-polynomial case; `Polynomial/Prelude.hs` provides `pseudoRemainder`, `classVarDeg`, `simplifyPolinomial`, `existOneDegPoly`.

4. **Theorem proving** — `Polynomial/TheoremProver.hs` (`theoremProver`). Reduces the conclusion against the Wu chain. A run is **Proved** iff the *last* pseudoremainder in the chain is zero (`Main.classify`). Intermediate zero remainders do not count.

`library/Core.hs` just re-exports these modules; the executable depends on `Core` alone.

## Runtime `n`, the type-level arity

The polynomial type is parameterized by a type-level `Nat` (number of total variables). `executable/Main.hs` reads `arNumXVars` at runtime, promotes it via `someNatVal`, and instantiates `runAt` under a `KnownNat n` proof — so any change to variable counting in `Tokenizer` must preserve the invariant `arNumXVars == length variables` or downstream pattern matches will silently truncate or pad polynomials. When editing across `Prelude.hs` / `Wu.hs` / `Tokenizer.hs`, keep the `KnownNat n` + `IsMonomialOrder n Grevlex` constraints threaded; GHC errors here are load-bearing.

## Outcome classification

`Main.Outcome`: `Proved | NotProved String | Errored String | TimedOut`. Each problem is run under `System.Timeout.timeout` wrapped in `try` over `evaluate` — parse errors, `Data.Ratio` zero-denominator blowups from `pseudoRemainder`, and timeouts all bucket separately from genuine FAILs. README.md documents the current baseline (1/30 proved) and the diagnosis order for improvements.

## Problems

- `problems/*.txt` — original AlphaGeometry natural-language statements.
- `problems/dsl/*.dsl` — best-effort DSL translations driven by the runner. Some (notably `imo_2011_p6`, `imo_2015_p4`, `imo_2020_p1`) are known-approximate; audit against the `.txt` before blaming the solver.
- `baselines/` — captured run outputs for comparison.
- `paper/` — research write-up (LaTeX).

## Code in English

`instructions` (repo root) requests English identifiers/comments. Existing code has Spanish holdovers (`simplifyPolinomial`, `Hipoteses`); prefer English for new code but do not mass-rename without being asked.
