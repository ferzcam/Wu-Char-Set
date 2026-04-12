# Wu-Char-Set

Haskell implementation of Wu's characteristic set method for automated
geometric theorem proving, targeting the IMO-AG-30 benchmark.

## What it does

Given a geometry problem expressed in a small DSL (triangles, circumcenters,
midpoints, foot-of-perpendicular, intersections, angle/length conditions, etc.),
the pipeline:

1. **Parses** the DSL into a list of construction steps.
2. **Algebraizes** each step into hypothesis polynomials over dependent
   coordinates `X` and free parameters `U`.
3. **Triangularizes** the hypothesis set via Wu's characteristic set
   construction (pseudoremainder chains).
4. **Reduces** the conclusion polynomial against the chain; a zero final
   pseudoremainder witnesses a proof.

The runner iterates every `.dsl` file under `problems/dsl/` and reports
`OK` / `FAIL` / `TIMEOUT` / `ERR` per problem along with timings.

## Repository layout

```
library/           Core solver (polynomial ring, Wu chain, theorem prover)
  Polynomial/      Prelude, Wu.hs, TheoremProver.hs
  Util/            Tokenizer (algebraization), Algebraizer (DSL -> hypotheses)
executable/        Main.hs — batch runner over problems/dsl/
problems/          IMO-AG-30 problem statements (.txt) and DSL files (.dsl)
  dsl/             DSL translations driven by the runner
test-suite/        Regression tests for the solver
paper/             Research write-up (main.tex / main.pdf)
baselines/         Captured run outputs for comparison
```

## Dependencies

- **GHC** (via Stack resolver `lts-18.21`)
- **Stack** build tool
- **computational-algebra** (konn/computational-algebra, pinned to commit
  `179c3617605c87daaac9f88ea81146c41e54f602` as a git extra-dep) — provides
  `halg-core`, `halg-polynomials`, multivariate polynomial rings, grevlex
  ordering, etc.
- Hackage extras: `algebra-4.3.1`, `control-monad-loop-0.1`, `unamb-0.2.7`
- Direct libraries used: `type-natural`, `singletons`, `random`, `containers`,
  `time`, `directory`, `filepath`

All extra-deps are pinned in `stack.yaml` — no manual setup required beyond
installing Stack.

## Installation

```bash
git clone <this-repo>
cd Wu-Char-Set
stack build
```

The first build fetches and compiles `computational-algebra` and its halg
sub-packages; expect a one-time cost of several minutes. Subsequent builds
are incremental.

## Usage

### Run every problem in `problems/dsl/`

```bash
stack run Wu-Char-Set
```

Each problem runs with a **30-second per-problem timeout** (default). Output
looks like:

```
problems/dsl/imo_2005_p5a.dsl ... 0.117s  OK  (nX=15)
problems/dsl/simson_line.dsl  ... 0.0s    FAIL: last pseudoremainder /= 0
problems/dsl/imo_2011_p6.dsl  ... 30.03s  TIMEOUT (>30s)
```

### Change the per-problem timeout

Pass `--timeout N` (seconds) as the first argument:

```bash
stack run Wu-Char-Set -- --timeout 10              # cap each problem at 10s
stack run Wu-Char-Set -- --timeout 120 --all       # 2-minute cap, full suite
```

### Run a subset of problems

Pass explicit DSL paths instead of running everything:

```bash
stack run Wu-Char-Set -- problems/dsl/simson_line.dsl problems/dsl/imo_2005_p5a.dsl
stack run Wu-Char-Set -- --timeout 5 problems/dsl/imo_2003_p4.dsl
```

### Run a different directory

```bash
stack run Wu-Char-Set -- --dir path/to/other/dsl
```

### Run the regression tests

```bash
stack test
```

## DSL reference

Each `.dsl` file is a sequence of construction steps followed by a single
`prove_*` line. Lines starting with `#` are comments.

| Step                              | Meaning                                        |
|-----------------------------------|------------------------------------------------|
| `triangle A B C`                  | `A` at origin, `B` on the x-axis, `C` free     |
| `circumcenter O A B C`            | `O` equidistant from `A`, `B`, `C`             |
| `orthocenter H A B C`             | `H` with `AH⊥BC` and `BH⊥AC`                   |
| `incenter I A B C`                | `I` on the `A`- and `C`-angle bisectors        |
| `parallelogram D A B C`           | `D` completes parallelogram `ABCD`             |
| `midpoint M A B`                  | `M` is the midpoint of `AB`                    |
| `foot F P A B`                    | `F` is the foot of perpendicular from `P` to `AB` |
| `mirror M P Q`                    | `M` is the reflection of `P` through `Q`       |
| `inter_ll I A B C D`              | `I = line(AB) ∩ line(CD)`                      |
| `inter_cl I O R A B`              | `I = circle(O through R) ∩ line(AB)`           |
| `inter_cc I O1 R1 O2 R2`          | `I = circle(O1 through R1) ∩ circle(O2 through R2)` |
| `on_line P A B`                   | `P` is a free point on line `AB` (1 DOF)       |
| `on_circle P O R`                 | `P` is a free point on circle(O through R) (1 DOF) |
| `free P`                          | `P` is an unconstrained free point (2 DOF)    |
| `perp A B C D`                    | constraint: `AB ⊥ CD`                          |
| `para A B C D`                    | constraint: `AB ∥ CD`                          |
| `cong A B C D`                    | constraint: `|AB| = |CD|`                      |
| `eqangle A B C D E F`             | constraint: `∠ABC = ∠DEF`                      |
| `prove_cong A B C D`              | goal: `|AB| = |CD|`                            |
| `prove_collinear A B C`           | goal: three points collinear                   |
| `prove_cyclic A B C D`            | goal: four points concyclic                    |
| `prove_para A B C D`              | goal: `AB ∥ CD`                                |
| `prove_perp A B C D`              | goal: `AB ⊥ CD`                                |
| `prove_eqangle A B C D E F`       | goal: `∠ABC = ∠DEF`                            |

See `library/Util/Algebraizer.hs` for the exact semantics of each step.

## Current baseline (IMO-AG-30)

Last captured run: **1 / 30 proved** (`imo_2005_p5a`), 8 timeouts at 30s,
2 errors, 19 fast fails. See the TODO section below — the instant-FAIL
bucket is largely explained by numeric U-value degeneracy rather than
algorithmic limitations, and is the main lever for improvement.

## TODO

Three investigations, in the recommended order:

### 1. Try different U-value substitutions (quick win, 5-minute change)

**What.** `library/Util/Tokenizer.hs` line 55 substitutes free parameters
with the fixed sequence `[0, 1, 4, 9, 16, 25, ...]` (squared integers).
This makes the solver *numerical* rather than symbolic: it proves the
theorem for one concrete instance of the free parameters. If that instance
happens to be degenerate (e.g. collinear, cocircular, zero leading
coefficient), the pseudoremainder chain can fail even when the theorem
is true over the generic point.

**Why first.** Classical results like `simson_line.dsl` currently fail
instantly despite being a textbook Wu success. If swapping the U sequence
(e.g. primes `[2, 3, 5, 7, 11, ...]` or random integers) flips `simson_line`
to `OK`, it confirms the pipeline is correct and the 19-strong instant-FAIL
bucket is largely a bad-instance artifact — expect a substantial OK count
jump with almost no effort. If nothing changes, rule (1) out and move to (2).

**How.** Edit the `monicPolys` definition in `library/Util/Tokenizer.hs`,
rebuild, rerun the subset. Worth iterating over a few sequences and taking
the union of proved problems as the baseline.

### 2. Track down the `Ratio has zero denominator` error (imo_2003_p4)

**What.** `imo_2003_p4` throws at runtime from `Data.Ratio`, meaning
something in the pseudoremainder pipeline divides by a zero polynomial
leading coefficient. Candidates: `simplifyPolinomial` (which likely makes
polynomials monic), `pseudoRemainder` / `findQR`, or the ratio arithmetic
on coefficients themselves.

**Why second.** It's a localized bug in one file, cheap to diagnose, and
may also be silently corrupting results on problems that don't throw
outright. Fixing it could unblock more than just 2003_p4.

**How.** Add a `try`/`evaluate` harness around each pseudoremainder step
with a label, rerun 2003_p4, see which term blows up. Or inspect
`simplifyPolinomial` for a division by leading coefficient and guard it.

### 3. Audit the DSL translations against their original statements

**What.** The 30 DSL files are best-effort translations from the
AlphaGeometry natural-language statements in `problems/*.txt`. Several
(notably `imo_2011_p6`, `imo_2015_p4`, `imo_2020_p1`) are compressed
approximations of very complex constructions and may differ materially
from the original theorems — they could be over-constrained, under-
constrained, or prove a different statement entirely.

**Why last.** Cheapest to defer until (1) and (2) are resolved, because
those affect *every* problem equally and will shrink the FAIL bucket
wholesale. Auditing individual DSL files only pays off once the
pipeline-level issues are fixed; doing it first risks spending time
fixing translations that would have worked with the right U values.

**How.** For each FAILing problem after (1) and (2): open the
corresponding `problems/*.txt`, read the original statement, verify the
DSL preserves the same hypotheses and conclusion. Cross-reference the
JGEX translations from Sinha et al. where available.

### Recommendation

Do **(1) first** — single-line change, potentially unlocks many problems.
Do **(2)** second — small, localized, and may be silently corrupting other
problems too. Leave **(3)** for last since it's the only per-problem effort
and benefits most from the prior two being resolved.
