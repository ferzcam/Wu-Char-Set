# Ritt-Wu Characteristic Set Method: Implementation Guide for Automated Theorem Proving

> **Purpose:** This document is a self-contained reference for implementing the Ritt-Wu Characteristic Set (CS) method in a functional language such as Haskell. It covers the three core components — pseudo-remainder computation, characteristic set construction, and the automated geometry theorem prover (AGTP) pipeline — at a level of detail sufficient to guide a complete implementation.

---

## Part I: Mathematical Foundations

### 1.1 The Polynomial Ring and Variable Ordering

All computation takes place in the ring $K[\mathbf{x}]$ where $K$ is a field of characteristic zero (typically $\mathbb{Q}$) and $\mathbf{x} = (x_1, x_2, \ldots, x_n)$ is an ordered tuple of variables with the fixed total ordering:

$$x_1 \prec x_2 \prec \cdots \prec x_n$$

This ordering is the single most important structural choice in the entire algorithm. Every subsequent definition — class, initial, ascending set, basic set — is relative to this ordering. **The ordering must be fixed before any computation begins and must never change during a single run.**

### 1.2 Polynomial Attributes (The Core Vocabulary)

For any non-zero polynomial $P \in K[\mathbf{x}]$, define the following attributes:

| Attribute | Notation | Definition |
|---|---|---|
| **Class** | $\text{cls}(P)$ | The largest index $c$ such that $x_c$ actually appears in $P$. For a non-zero constant $P \in K$, $\text{cls}(P) = 0$. |
| **Leading Variable** | $\text{lv}(P)$ | The variable $x_c$ where $c = \text{cls}(P)$. Undefined for constants. |
| **Leading Degree** | $\text{ldeg}(P)$ | $\deg(P, \text{lv}(P))$, i.e., the degree of $P$ in its leading variable. For constants, $\text{ldeg}(P) = 0$. |
| **Initial** | $\text{ini}(P)$ | The leading coefficient of $P$ viewed as a univariate polynomial in $\text{lv}(P)$. This is a polynomial in $K[x_1, \ldots, x_{c-1}]$. For constants, $\text{ini}(P) = P$. |
| **Reductum** | $\text{red}(P)$ | $P - \text{ini}(P) \cdot \text{lv}(P)^{\text{ldeg}(P)}$, i.e., $P$ with its leading term removed. |
| **Separant** | $\text{sep}(P)$ | The formal partial derivative $\partial P / \partial \text{lv}(P)$. |

**Normal form of a non-constant polynomial** $P$ of class $c$ and leading degree $d$:

$$P = I \cdot x_c^d + \text{(lower degree terms in } x_c\text{)}$$

where $I = \text{ini}(P) \in K[x_1, \ldots, x_{c-1}]$.

### 1.3 Orderings on Polynomials and Ascending Sets

**Ordering on polynomials.** $P$ is of *lower ordering* than $Q$ (written $P \prec Q$) if:
- $\text{cls}(P) < \text{cls}(Q)$, **or**
- $\text{cls}(P) = \text{cls}(Q)$ and $\text{ldeg}(P) < \text{ldeg}(Q)$.

Two polynomials $P \sim Q$ if they have the same class and leading degree. This defines a partial order on $K[\mathbf{x}]$.

**Reduced.** A polynomial $Q$ is *reduced* with respect to $P$ (with $\text{cls}(P) = c > 0$) if $\deg(Q, x_c) < \text{ldeg}(P)$.

**Ascending Set.** A finite set $AS = \{A_1, A_2, \ldots, A_r\}$ of non-zero polynomials, arranged so that:
$$\text{cls}(A_1) < \text{cls}(A_2) < \cdots < \text{cls}(A_r)$$
and for each $i < j$, $\text{ini}(A_j)$ is reduced with respect to $A_i$.

A weaker form — the **triangular set** (or *weak ascending set*) — requires only $\text{cls}(A_1) < \cdots < \text{cls}(A_r)$ without the condition on initials.

**Ordering on Ascending Sets.** Given two ascending sets $AS'$ and $AS''$, $AS' \prec AS''$ if one of:
- $(o_1)$: There exists $k$ such that $A'_1 \sim A''_1, \ldots, A'_{k-1} \sim A''_{k-1}$, while $A'_k \prec A''_k$.
- $(o_2)$: $r' > r''$ and $A'_1 \sim A''_1, \ldots, A'_{r''} \sim A''_{r''}$.

This is a **well-ordering**: any strictly decreasing sequence of ascending sets is finite (Lemma 1.2 in Wu 2000). This finiteness is the termination guarantee for the entire algorithm.

**Basic Set.** A *basic set* of a polynomial set $PS$ is any ascending set of lowest ordering contained in $PS$. Any two basic sets of the same polynomial set have the same ordering.

---

## Part II: Pseudo-Remainder Computation

### 2.1 Single-Step Pseudo-Remainder

The pseudo-remainder is the fundamental elimination operation. It generalises Euclidean division to polynomial rings where the leading coefficient of the divisor may not divide the dividend.

**Definition.** Let $F, G \in K[\mathbf{x}]$ with $c = \text{cls}(F)$ and $I = \text{ini}(F)$, $d = \text{ldeg}(F)$. The *pseudo-remainder* $R = \text{prem}(G, F)$ is the unique polynomial reduced with respect to $F$ satisfying:

$$I^s \cdot G = Q \cdot F + R$$

for some polynomial $Q$ and the smallest non-negative integer $s$ that makes this hold. $R$ is uniquely determined.

**Algorithm for $\text{prem}(G, F, x_c)$:**

```
prem(G, F, x_c):
  I  := ini(F)
  d  := ldeg(F)
  R  := G
  while deg(R, x_c) >= d:
    delta := deg(R, x_c) - d
    lc_R  := leading coefficient of R in x_c
    R     := I * R - lc_R * x_c^delta * F
  return R
```

The loop invariant is that each iteration reduces $\deg(R, x_c)$ by at least one. Termination is guaranteed because degree is a non-negative integer that strictly decreases.

**Key property:** The result $R$ satisfies $\deg(R, x_c) < d = \text{ldeg}(F)$, so $R$ is reduced with respect to $F$ in the variable $x_c$.

**Note on "Ritt's sense" vs standard pseudo-remainder.** The standard pseudo-remainder multiplies by the minimum power of $I$ needed: $I^{\deg(G, x_c) - d + 1} \cdot G = Q \cdot F + R$. Ritt's version uses the *smallest* $s$ that makes the formula valid (which may be smaller). For practical implementation, the standard formula is simpler and correct; the distinction does not affect the zero-set properties.

### 2.2 Iterated Pseudo-Remainder over a Triangular Set

For a triangular set $TS = [T_1, T_2, \ldots, T_r]$ (ordered ascending by class) and any polynomial $G$, the iterated pseudo-remainder is:

$$\text{prem}(G, TS) := \text{prem}(\cdots \text{prem}(\text{prem}(G, T_r), T_{r-1}), \cdots, T_1)$$

Processing proceeds from **highest class to lowest class**. After each reduction by $T_i$, the degree in $\text{lv}(T_i)$ is strictly less than $\text{ldeg}(T_i)$.

**The remainder formula.** There exist polynomials $Q_i$ and a product-of-initials $I_G$ such that:

$$I_G \cdot G = \sum_{i=1}^{r} Q_i T_i + R, \quad R = \text{prem}(G, TS)$$

The uniquely determined $R$ is reduced with respect to every polynomial in $TS$.

### 2.3 Worked Example

**Setting:** Variable order $x_0 \prec x_1$. Compute $\text{prem}(x_1^2 - x_0,\ x_1 - x_0)$.

```
F = x_1 - x_0,    cls(F) = 1,    ini(F) = 1,    ldeg(F) = 1
G = x_1^2 - x_0,  deg(G, x_1)  = 2

Iteration 1: deg = 2 >= 1, delta = 1, lc_G = 1
  R = 1*(x_1^2 - x_0) - 1 * x_1^1 * (x_1 - x_0)
    = x_1^2 - x_0 - x_1^2 + x_0*x_1
    = x_0*x_1 - x_0

Iteration 2: deg = 1 >= 1, delta = 0, lc_R = x_0
  R = 1*(x_0*x_1 - x_0) - x_0 * x_1^0 * (x_1 - x_0)
    = x_0*x_1 - x_0 - x_0*x_1 + x_0^2
    = x_0^2 - x_0

Result: prem(x_1^2 - x_0,  x_1 - x_0) = x_0^2 - x_0 = x_0(x_0 - 1)
```

**Interpretation:** The result $x_0(x_0 - 1)$ vanishes exactly when $x_0 = 0$ or $x_0 = 1$, which are exactly the intersection points of the parabola $x_1 = x_0^2$ and the line $x_1 = x_0$.

### 2.4 Computational Considerations

- **Coefficient swell.** Repeated pseudo-division can cause exponential growth in coefficient size. In practice, use GCD-based content reduction after each division step, or use **subresultant sequences** (see Section 6.2) which guarantee polynomial-size intermediate results.
- **Zero detection.** Always discard any remainder that reduces to zero from the remainder set. A remainder of zero means the polynomial is already in the ideal generated by the triangular set.
- **Constant remainders.** If $\text{prem}(G, TS) \in K \setminus \{0\}$ (a non-zero constant), then $G$ is not in $\text{sat}(TS)$, meaning the geometric or algebraic statement fails completely on that component.

---

## Part III: The Characteristic Set Algorithm

### 3.1 Definitions

**Characteristic Set.** An ascending set $CS$ obtained from a polynomial set $PS$ via the procedure in Scheme (S) below is a *characteristic set* of $PS$. More generally, any ascending set $CS$ satisfying both of the following properties is a characteristic set of $PS$:

- $(c_1)$: $\text{prem}(PS, CS) = \{0\}$ — every polynomial in $PS$ pseudo-reduces to zero modulo $CS$.
- $(c_2)$: $\text{Zero}(PS) \subseteq \text{Zero}(CS)$ — any common zero of $PS$ is also a zero of $CS$.

### 3.2 The Construction Scheme (S)

```
charset(PS):
  PS_0 := PS
  BS_0 := selectBasicSet(PS_0)      -- lowest-ordering ascending set in PS_0
  RS_0 := { prem(P, BS_0) | P ∈ PS_0 \ BS_0, prem(P, BS_0) ≠ 0 }

  if RS_0 = ∅:
    return BS_0                      -- BS_0 is the characteristic set

  PS_1 := PS_0 ∪ BS_0 ∪ RS_0       -- standard variant
  -- OR: PS_1 := BS_0 ∪ RS_0        -- efficient variant (may yield different CS)

  return charset(PS_1)
```

**In tabular form (Scheme S):**

$$\begin{array}{cccccc}
PS = & PS_0 & PS_1 & \cdots & PS_i & \cdots & PS_m \\
     & BS_0 & BS_1 & \cdots & BS_i & \cdots & BS_m = CS \\
     & RS_0 & RS_1 & \cdots & RS_i & \cdots & RS_m = \varnothing
\end{array}$$

### 3.3 Termination Proof Sketch

By **Lemma 1.1**: adjoining a polynomial reduced with respect to an ascending set $BS_i$ (which all elements of $RS_i$ are, by construction) to $BS_i$ yields a new ascending set of **strictly lower ordering** than $BS_i$.

Therefore $BS_0 \succ BS_1 \succ BS_2 \succ \cdots$, a strictly decreasing sequence of ascending sets.

By **Lemma 1.2** (the well-ordering principle): any strictly decreasing sequence of ascending sets over a Noetherian ring is **finite**.

Therefore the algorithm terminates at some stage $m$ where $RS_m = \varnothing$.

### 3.4 The `selectBasicSet` Subroutine

This is the most algorithmically subtle component. It must find an ascending set of **minimal ordering** within a given polynomial set.

**Algorithm sketch:**

```
selectBasicSet(PS):
  -- Group PS by class
  for each class c from 1 to n:
    candidates := { P ∈ PS | cls(P) = c }
    if candidates ≠ ∅:
      pick T_c := element of candidates with minimum ldeg in x_c
      -- T_c is the representative for class c

  -- Build ascending set by checking the initial condition
  AS := []
  for each chosen T_c in increasing class order:
    if ini(T_c) is reduced with respect to all polynomials already in AS:
      AS := AS ++ [T_c]

  return AS
```

**Important:** The initial condition (that $\text{ini}(A_j)$ is reduced w.r.t. $A_i$ for $i < j$) distinguishes a true ascending set from a mere triangular set. If this condition fails, split the polynomial set or use a triangular set as an approximation. Many practical implementations use triangular sets (dropping the initial condition) for efficiency.

### 3.5 Properties of the Resulting Characteristic Set

**Theorem (Wu 2000, Theorem 1.3 and 1.4).** Let $CS$ be the characteristic set of $PS$ produced by Scheme (S). Then:

1. **Zero remainder:** $\text{prem}(P, CS) = 0$ for every $P \in PS$.
2. **Zero inclusion:** $\text{Zero}(PS) \subseteq \text{Zero}(CS)$.
3. **Well-ordering principle (full form):**

$$\text{Zero}(CS/IP) \subseteq \text{Zero}(PS) \subseteq \text{Zero}(CS)$$

$$\text{Zero}(PS) = \text{Zero}(CS/IP) \cup \bigcup_i \text{Zero}(PS \cup \{I_i\})$$

where $I_i$ is the initial of the $i$-th polynomial in $CS$ and $IP = \prod_i I_i$ is the *initial product*.

**Dimension and Degree.** A characteristic set $CS = \{C_1, \ldots, C_r\}$ characterises the algebraic variety via:
- **Dimension:** The number of variables not appearing as leading variables in $CS$ (the *parameters* or *free variables*).
- **Degree:** $\prod_{i=1}^r \text{ldeg}(C_i)$, the product of leading degrees.

### 3.6 Zero Decomposition Theorems

For a more refined analysis (needed in theorem proving), apply the well-ordering principle recursively:

**Theorem (Zero Decomposition I).** For any polynomial set $PS$, there exists a finite set of ascending sets $\{CS_k\}$ such that:

$$\text{Zero}(PS) = \bigcup_k \text{Zero}(CS_k / IP_k)$$

where $IP_k$ is the initial product of $CS_k$, and $\text{prem}(PS, CS_k) = \{0\}$ for all $k$.

**Algorithm:** Compute $CS$ of $PS$. Apply the well-ordering principle to split on each initial $I_i$: for each $i$, recursively compute the characteristic set of $PS \cup \{I_i\}$. Repeat until all components have empty remainder sets.

**Saturation ideal.** For a triangular set $TS$, define:
$$\text{sat}(TS) := \text{Ideal}(TS) : \left(\prod_{T \in TS} \text{ini}(T)\right)^\infty$$

A regular set $TS$ satisfies $\text{sat}(TS) = \{P \in K[\mathbf{x}] \mid \text{prem}(P, TS) = 0\}$.

---

## Part IV: Types of Triangular Systems

Understanding the hierarchy of triangular systems is essential for choosing the right algorithm variant.

### 4.1 Hierarchy

```
Fine Triangular System
  └── Regular System         (sat(TS) well-behaved; prem characterises sat)
        └── Simple System    (squarefree; strong projection property)
              └── Irreducible System  (each polynomial irreducible over param field)
```

| Type | Key Property | Membership Test |
|---|---|---|
| **Fine** | $0 \notin \text{prem}(US, TS)$ | No inequation is eliminated by TS |
| **Regular** | $\text{sat}(TS) = \{P \mid \text{prem}(P,TS) = 0\}$ | $\text{res}(\text{ini}(T_{i+1}), TS^{(i)}) \neq 0$ for all $i$ |
| **Simple** | Each poly squarefree in its leading variable | $\gcd(P, \partial P/\partial x_c) \in K$ |
| **Irreducible** | Each $T_i$ irreducible over $K_{i-1}(\eta_1,\ldots,\eta_{i-1})$ | Requires polynomial factorisation |

For automated theorem proving, **regular systems** are the practical minimum: they guarantee that $\text{prem}(C, TS) = 0$ if and only if $C$ vanishes on all regular zeros of $TS$ (up to a power, via Theorem 2.15). **Simple and irreducible systems** give the exact criterion without powers.

### 4.2 Fine Triangular Systems: Algorithm P and T

**Algorithm P** (Principal triangular system) computes a single fine triangular system from a polynomial system $[PS, QS]$ by top-down elimination from $x_n$ down to $x_1$:

```
AlgorithmP([PS, QS]):
  TS := ∅;  FS := PS;  US := QS;  Ω := ∅

  for i = level(PS) down to 1:
    if FS ∩ K\{0} ≠ ∅:  return [1]    -- no solution
    if level(FS) < i:   continue

    T := element of FS^(i) with minimal degree in x_i
    while FS^(i) ≠ {T}:
      -- Splitting step (Lemma 2.2):
      Ω := Ω ∪ { [FS\{T} ∪ {red(T), ini(T)}, US, TS] }
      US := US ∪ { ini(T) }
      -- Reduction step:
      FS := {T} ∪ prem(FS, T) \ {0}

    FS := FS \ {T}
    US := prem(US, T)
    if 0 ∈ US:  return [1]
    TS := [T] ∪ TS

  return [TS, US]
```

**Algorithm T** applies Algorithm P recursively to all generated triplets in $\Omega$, collecting all fine triangular systems into a series $\Psi$ such that:

$$\text{Zero}(PS/QS) = \bigcup_{\mathfrak{T} \in \Psi} \text{Zero}(\mathfrak{T})$$

### 4.3 Algorithm T* (Subresultant-Based)

Algorithm T* replaces the inner pseudo-division loop of Algorithm P with **Subresultant Regular Subchain (SRS)** computation. Given two polynomials $P_1, P_2$ with $\deg(P_1, x_k) \geq \deg(P_2, x_k) > 0$, the SRS $H_2, \ldots, H_r$ of $P_1$ and $P_2$ satisfies (Lemma 2.3):

$$\text{Zero}(\{P_1, P_2\}/I) = \bigcup_{i=2}^{r} \text{Zero}(\{H_i, I_{i+1}, \ldots, I_r\}/II_i)$$

where $I = \text{lc}(P_2, x_k)$ and $I_i = \text{lc}(H_i, x_k)$.

**Why T* matters:** Standard pseudo-division causes exponential coefficient growth. SRS-based elimination maintains **polynomial-size** intermediate results. For any non-trivial implementation, T* is strongly preferred over T.

---

## Part V: The Automated Geometry Theorem Prover

### 5.1 Overview of Wu's Method

Wu's AGTP method (introduced 1978, formalised 1984) reduces geometric theorem proving to polynomial pseudo-remainder computation. It has been used to prove over 600 geometric theorems automatically, including the discovery of previously unknown results.

The method applies to theorems of **equality type**: given geometric hypotheses expressible as polynomial equations $H_i = 0$, decide whether a conclusion $C = 0$ follows.

### 5.2 The Three-Step Pipeline

#### Step W1: Algebraisation (Geometry → Polynomials)

Choose a coordinate system. Assign coordinates to all geometric points and entities as indeterminates $y_1, \ldots, y_m$. Translate:

- Each geometric hypothesis (collinearity, perpendicularity, equal lengths, etc.) into a polynomial equation $H_i(y_1, \ldots, y_m) = 0$.
- The geometric conclusion into a polynomial equation $C(y_1, \ldots, y_m) = 0$.

**Variable classification.** Partition the indeterminates into:
- **Parameters** $u_1, \ldots, u_d$: freely chosen (e.g., coordinates of fixed points set to specific values or left free).
- **Dependents** $y_1, \ldots, y_r$: determined by the hypothesis equations given the parameters.

The variable ordering must place parameters before dependents: $u_1 \prec \cdots \prec u_d \prec y_1 \prec \cdots \prec y_r$.

**Standard geometric translations:**

| Geometric condition | Polynomial equation |
|---|---|
| $A, B, C$ collinear ($A=(a_1,a_2), B=(b_1,b_2), C=(c_1,c_2)$) | $(b_1-a_1)(c_2-a_2) - (b_2-a_2)(c_1-a_1) = 0$ |
| $AB \perp CD$ | $(b_1-a_1)(d_1-c_1) + (b_2-a_2)(d_2-c_2) = 0$ |
| $AB \parallel CD$ | $(b_1-a_1)(d_2-c_2) - (b_2-a_2)(d_1-c_1) = 0$ |
| $\|AB\| = \|CD\|$ | $(b_1-a_1)^2+(b_2-a_2)^2 - (d_1-c_1)^2-(d_2-c_2)^2 = 0$ |
| $O$ is the intersection of lines $AB$ and $CD$ | Two collinearity equations for $O$ |

#### Step W2: Triangulation (Polynomials → Characteristic Set)

Apply Scheme (S) (or Algorithm P/T) to the hypothesis polynomial set $HS = \{H_1, \ldots, H_s\}$ to obtain a characteristic set $CS = \{F_1, \ldots, F_p\}$.

The polynomials in $CS$ are ordered by class: $\text{cls}(F_1) < \text{cls}(F_2) < \cdots < \text{cls}(F_p)$.

This step transforms the unstructured hypothesis system into a triangular form where each $F_i$ introduces exactly one new dependent variable of highest class.

#### Step W3: Successive Division (Verification)

Compute the final pseudo-remainder:

$$R = \text{prem}(C, CS) = \text{prem}(\cdots\text{prem}(\text{prem}(C, F_p), F_{p-1})\cdots, F_1)$$

**Decision rule:**

| Result | Conclusion |
|---|---|
| $R = 0$ | Theorem is **valid** under the non-degeneracy conditions $I_k \neq 0$ (where $I_k = \text{ini}(F_k)$). |
| $R \neq 0$ | Theorem is **generally false**, or the system is incomplete / missing hypotheses. |

### 5.3 Non-Degeneracy Conditions

When $R = 0$, the theorem is proved under the *subsidiary conditions* that the initials of $CS$ are non-zero: $I_1 \neq 0, \ldots, I_p \neq 0$. These conditions exclude degenerate geometric configurations (e.g., a triangle collapsing to a line, two distinct points coinciding).

The conditions $I_k \neq 0$ are generated **automatically** by the algorithm — no user input is required. They are the algebraic encoding of "the geometric configuration is in general position."

**What happens when an initial vanishes?** If $I_k = 0$ at some point, it means the configuration has degenerated. To analyse these cases, apply the zero decomposition theorem recursively: compute the characteristic set of $HS \cup \{I_k\}$ and check whether $C = 0$ holds on that component.

### 5.4 Worked Example: Desargues' Theorem

**Statement.** Let $A_1B_1C_1$ and $A_2B_2C_2$ be two triangles with corresponding sides parallel. If $A_1A_2$ and $B_1B_2$ intersect at $O$, then $C_1C_2$ also passes through $O$.

**Step W1 — Algebraisation.** Set $O = (0,0)$, $A_1 = (x_1, 0)$, $A_2 = (x_2, 0)$, $B_1 = (x_3, x_4)$, $B_2 = (x_5, x_6)$, $C_1 = (x_7, x_8)$, $C_2 = (x_9, x_{10})$.

Hypotheses (parallel sides, $O$ on $A_1A_2 \cap B_1B_2$):

$$H_1 = x_4(x_5 - x_2) - x_6(x_3 - x_1) = 0 \quad (A_1B_1 \parallel A_2B_2)$$
$$H_2 = x_8(x_9 - x_2) - x_{10}(x_7 - x_1) = 0 \quad (A_1C_1 \parallel A_2C_2)$$
$$H_3 = (x_4 - x_8)(x_5 - x_9) - (x_6 - x_{10})(x_3 - x_7) = 0 \quad (B_1C_1 \parallel B_2C_2)$$
$$H_4 = x_6 x_3 - x_5 x_4 = 0 \quad (O = A_1A_2 \cap B_1B_2)$$

Conclusion ($C_1C_2$ passes through $O$):
$$C = x_9 x_8 - x_{10} x_7 = 0$$

**Step W2 — Triangulation.** Compute $CS = \{F_1, F_2, F_3, F_4\}$ from $HS = \{H_1, H_2, H_3, H_4\}$ by Scheme (S). (The explicit polynomials are degree 1–2 in the dependent variables.)

**Step W3 — Division.** Computing $\text{prem}(C, CS)$ yields $R = 0$.

**Conclusion.** The theorem is valid under the non-degeneracy conditions $DS = \{x_1, x_3, x_4, -x_7 + x_1, x_4 x_7 - x_3 x_8 - x_4 x_1 + x_1 x\}$, which exclude configurations where $O = A_1$ (degenerate triangle), $A_1 = B_1$, etc.

### 5.5 Complete AGTP Framework: Three Formulations

Depending on how non-degeneracy is handled, there are three formulations with different precision/cost tradeoffs:

**Formulation I (Generically True — Wu's original).** Use decomposition (5.1): decompose $\text{Zero}(HS)$ into irreducible components. Identify the *non-degenerate* components (those where the parameters are algebraically independent). The theorem is *generically true* if $\text{prem}(C, AS_i) = 0$ for all non-degenerate components $AS_i$.

**Formulation II (Exact non-degeneracy).** The user specifies explicit non-degeneracy conditions $D_1 \neq 0, \ldots, D_q \neq 0$ as part of the hypothesis. Prove:
$$\text{Zero}(PS/QS) \subseteq \text{Zero}(C)$$
by verifying $\text{prem}(C, CS) = 0$ where $CS$ is computed from $[PS, QS]$.

**Formulation III (Full component analysis — Wang 1995).** Allow both equations and inequations in hypotheses. Decompose $\text{Zero}(PS/QS)$ into irreducible components. Check $\text{prem}(C, AS_i) = 0$ for each component and report on which components the theorem holds and which it fails. This gives a complete algebraic analysis.

**Practical recommendation.** For most geometric theorems, Formulation I (Wu's original simple version) suffices and is most efficient. Use Formulation III when a theorem is known to be reducible (multiple configurations, ambiguous algebraic encoding of geometric relationships).

### 5.6 Mechanical Formula Derivation (Theorem Discovery)

Beyond proving given theorems, the CS method can **discover** unknown geometric relationships.

**Problem.** Given a geometric configuration encoded as $HS$, find the algebraic relationship among certain entities $x_1, \ldots, x_k$ (e.g., the three sides and area of a triangle).

**Method.** Order variables so that the entities of interest $x_1, \ldots, x_k$ come **first** in the variable ordering. Compute the characteristic set $CS$ of $HS$. By the well-ordering principle:

$$\text{Zero}(HS) \subseteq \text{Zero}(CS)$$

The first polynomial $C_1 \in CS$ involves only the lowest-class variables — precisely the entities of interest — and $C_1 = 0$ is the sought-for relationship.

**Example (Heron's formula).** With $x_1 = |AB|$, $x_2 = |AC|$, $x_3 = |BC|$, $x_4 = \text{area}(ABC)$ ordered first, the characteristic set of the coordinate-based hypothesis equations yields:

$$C_1 = 2x_1^2 x_2^2 + 2x_1^2 x_3^2 + 2x_2^2 x_3^2 - x_1^4 - x_2^4 - x_3^4 + 16x_4^2 = 0$$

This is exactly Heron's formula: $16S^2 = (a+b+c)(-a+b+c)(a-b+c)(a+b-c)$.

---

## Part VI: Implementation Guide for Haskell

### 6.1 Core Type Definitions

```haskell
-- Variable index (1-indexed, matching the ordering x_1 < x_2 < ... < x_n)
type VarIdx = Int

-- Monomial: a map from variable index to exponent
type Monomial = Map VarIdx Int

-- Polynomial: a map from monomial to rational coefficient
-- Use Ratio Integer for exact arithmetic
type Polynomial = Map Monomial (Ratio Integer)

-- Ascending set: ordered list of polynomials, each with strictly higher class
newtype AscendingSet = AscendingSet [Polynomial]
  deriving (Show, Eq)

-- Triangular set (weaker: no initial condition required)
newtype TriangularSet = TriangularSet [Polynomial]
  deriving (Show, Eq)

-- Polynomial system: equations and inequations
data PolySystem = PolySystem
  { equations   :: [Polynomial]  -- must all be zero
  , inequations :: [Polynomial]  -- must all be nonzero
  } deriving (Show)
```

### 6.2 Polynomial Attribute Functions

```haskell
-- Class: largest variable index with positive exponent
cls :: Polynomial -> Int
cls p
  | Map.null p = 0
  | otherwise  = maximum $ concatMap (Map.keys) (Map.keys p)

-- Leading degree: degree in the leading variable
ldeg :: Polynomial -> Int
ldeg p =
  let c = cls p
  in maximum [ Map.findWithDefault 0 c m | m <- Map.keys p ]

-- Initial: leading coefficient as polynomial in x_1,...,x_{c-1}
ini :: Polynomial -> Polynomial
ini p =
  let c  = cls p
      d  = ldeg p
      -- Keep only terms with degree d in x_c, then remove x_c from monomial
  in Map.fromListWith (+)
       [ (Map.delete c m, coef)
       | (m, coef) <- Map.toList p
       , Map.findWithDefault 0 c m == d
       ]

-- Reductum: P minus its leading term
red :: Polynomial -> Polynomial
red p =
  let c = cls p
      d = ldeg p
  in Map.filterWithKey (\m _ -> Map.findWithDefault 0 c m < d) p

-- Check if Q is reduced with respect to P (deg(Q, lv(P)) < ldeg(P))
isReducedWrt :: Polynomial -> Polynomial -> Bool
isReducedWrt q p =
  let c = cls p
      d = ldeg p
  in all (\m -> Map.findWithDefault 0 c m < d) (Map.keys q)
```

### 6.3 Pseudo-Remainder

```haskell
-- Single-step pseudo-remainder of G by F with respect to lv(F)
pseudoRem :: Polynomial -> Polynomial -> Polynomial
pseudoRem g f
  | isReducedWrt g f = g
  | otherwise =
      let c      = cls f
          d      = ldeg f
          initF  = ini f
          degG   = maximum [ Map.findWithDefault 0 c m | m <- Map.keys g ]
          delta  = degG - d
          -- Leading coefficient of G in x_c
          lcG    = Map.fromListWith (+)
                     [ (Map.delete c m, coef)
                     | (m, coef) <- Map.toList g
                     , Map.findWithDefault 0 c m == degG
                     ]
          -- Scaled divisor term: lc_G * x_c^delta * f
          scaledF = Map.fromListWith (+)
                      [ (Map.insertWith (+) c delta m, coef1 * coef2)
                      | (m1, coef1) <- Map.toList lcG
                      , (m2, coef2) <- Map.toList f
                      , let m = Map.unionWith (+) m1 m2
                      ]
          -- New remainder: ini(F) * G - lc_G * x_c^delta * F
          g' = subtractPoly (scalePoly initF g) scaledF
      in pseudoRem g' f

-- Iterated pseudo-remainder over a triangular set (highest class first)
iteratedPrem :: Polynomial -> [Polynomial] -> Polynomial
iteratedPrem p ts =
  -- Sort ts by descending class, then fold
  foldl (\acc t -> if cls acc > cls t || (cls acc == cls t)
                   then pseudoRem acc t
                   else acc)
        p
        (sortBy (comparing (negate . cls)) ts)
```

### 6.4 Ascending Set Ordering

```haskell
-- Compare two polynomials: (class, ldeg) lexicographically
polyOrder :: Polynomial -> Polynomial -> Ordering
polyOrder p q = compare (cls p, ldeg p) (cls q, ldeg q)

-- Compare two ascending sets (lexicographic on element ordering)
ascSetOrder :: AscendingSet -> AscendingSet -> Ordering
ascSetOrder (AscendingSet ps) (AscendingSet qs) = go ps qs
  where
    go [] [] = EQ
    go [] _  = GT   -- ps is shorter => ps < qs in Wu's ordering (o_2)
    go _  [] = LT
    go (p:ps') (q:qs') = case polyOrder p q of
      EQ -> go ps' qs'
      r  -> r
```

### 6.5 Select Basic Set

```haskell
-- Find the ascending set of lowest ordering in a polynomial set
selectBasicSet :: [Polynomial] -> AscendingSet
selectBasicSet ps =
  let nonConstants = filter (\p -> cls p > 0) ps
      byClass      = groupBy (\a b -> cls a == cls b)
                   . sortBy (comparing cls)
                   $ nonConstants
      -- For each class, pick the polynomial of minimal leading degree
      candidates   = map (minimumBy (comparing ldeg)) byClass
      -- Build ascending set: enforce initial condition
      buildAS []     acc = acc
      buildAS (c:cs) acc =
        if all (isReducedWrt (ini c)) acc
        then buildAS cs (acc ++ [c])
        else buildAS cs acc  -- skip if initial condition fails
  in AscendingSet (buildAS candidates [])
```

### 6.6 Characteristic Set (Main Algorithm)

```haskell
-- Compute a characteristic set of a polynomial set
characteristicSet :: [Polynomial] -> AscendingSet
characteristicSet ps = go (filter (not . isZero) ps)
  where
    go ps0 =
      let AscendingSet bs = selectBasicSet ps0
          nonMembers = filter (`notElem` bs) ps0
          rs = filter (not . isZero)
               [ iteratedPrem p bs | p <- nonMembers ]
      in if null rs
         then AscendingSet bs          -- termination: RS is empty
         else go (ps0 ++ bs ++ rs)     -- standard variant
                 -- OR: go (bs ++ rs)  -- efficient variant
```

### 6.7 AGTP Top-Level Functions

```haskell
-- Translate geometric theorem to polynomial system (user-provided)
-- Returns (hypotheses, conclusion, variable count)
type GeomTheorem = ([Polynomial], Polynomial, Int)

-- Wu's AGTP: returns (isValid, nonDegeneracyConditions)
wuAGTP :: GeomTheorem -> (Bool, [Polynomial])
wuAGTP (hypotheses, conclusion, _) =
  let AscendingSet cs = characteristicSet hypotheses
      remainder       = iteratedPrem conclusion cs
      initials        = map ini cs
  in (isZero remainder, initials)

-- Full analysis: check remainder against each component of zero decomposition
wuAGTPFull :: GeomTheorem -> [(AscendingSet, Bool)]
wuAGTPFull (hypotheses, conclusion, _) =
  let components = zeroDecomposition hypotheses
  in [ (as, isZero (iteratedPrem conclusion (unAS as)))
     | as <- components
     ]
  where unAS (AscendingSet ps) = ps
```

### 6.8 Implementation Checklist

| Component | Notes |
|---|---|
| `Polynomial` type | Sparse `Map Monomial (Ratio Integer)` — exact arithmetic is mandatory |
| `cls`, `ldeg`, `ini`, `red` | Core attribute functions; test thoroughly on edge cases |
| `pseudoRem` | Single-step; the inner loop must strictly decrease degree |
| `iteratedPrem` | Reduce w.r.t. each element of TS from highest to lowest class |
| `polyOrder`, `ascSetOrder` | Must implement Wu's specific ordering (not degree-lex) |
| `selectBasicSet` | Most subtle subroutine; consider triangular sets for simplicity |
| `characteristicSet` | Main loop; add size/depth limit as safety during debugging |
| `wuAGTP` | Top-level prover; returns Bool + non-degeneracy initials |
| Coefficient arithmetic | Use `Data.Ratio` or `Data.Map` with GCD reduction; avoid floating point |
| SRS (optional) | Implement for production use to avoid coefficient explosion |

### 6.9 Recommended Libraries

- **`containers`** (`Data.Map.Strict`, `Data.Set`) — for sparse polynomial representation.
- **`base` `Data.Ratio`** — exact rational arithmetic.
- **`QuickCheck`** — property-based testing of algebraic identities.
- **`polynomial`** (Hackage) — if available; or implement from scratch for full control.
- **`sbv`** — for validating zero-set containment on concrete instances.

---

## Part VII: Worked System Example

### Unit Circle ∩ Line ($x^2 + y^2 = 1$, $x = y$)

**Variable order:** $x \prec y$.

**Polynomial set:**
$$PS = \{f_1, f_2\} = \{x^2 + y^2 - 1,\ x - y\}$$

**Classes:**
- $f_1$: $\text{cls} = y$, $\text{ldeg} = 2$, $\text{ini}(f_1) = 1$
- $f_2$: $\text{cls} = y$, $\text{ldeg} = 1$, $\text{ini}(f_2) = -1$

**Step 1 — Select Basic Set $BS_0$.**
Both polynomials have class $y$. Pick $f_2$ (lower degree in $y$): $BS_0 = \{f_2\} = \{x - y\}$.

**Step 2 — Compute $RS_0$.**
Reduce $f_1$ w.r.t. $BS_0$:
$$\text{prem}(x^2 + y^2 - 1,\ x - y,\ y)$$

$\text{ini}(f_2) = -1$, $\text{ldeg}(f_2) = 1$, $\text{deg}(f_1, y) = 2$.

- Step 1: eliminate $y^2$: $R = (-1)(x^2 + y^2 - 1) - (-y)(x - y) = -x^2 - y^2 + 1 - (-xy + y^2) = -x^2 + xy - 2y^2 + 1$... 

Using the standard formula directly: multiply $f_1$ by $(-1)^{2-1+1} = 1$, then reduce:
$$(-1)^2 \cdot f_1 = x^2 + y^2 - 1$$
Eliminate $y^2$: subtract $(-y) \cdot f_2 = -y(x-y) = -xy + y^2$, giving $x^2 + xy - 1$.  
Eliminate $y^1$: subtract $(-x) \cdot f_2 = -x(x-y) = -x^2 + xy$, giving $2x^2 - 1$.

$$RS_0 = \{2x^2 - 1\}$$

**Step 3 — New polynomial set $PS_1 = BS_0 \cup RS_0$.**
$$PS_1 = \{x - y,\ 2x^2 - 1\}$$

**Step 4 — Select Basic Set $BS_1$.**
- $2x^2 - 1$: $\text{cls} = x$, $\text{ldeg} = 2$
- $x - y$: $\text{cls} = y$, $\text{ldeg} = 1$

Both have different classes → both enter the ascending set:
$$BS_1 = \{2x^2 - 1,\ x - y\}$$

**Step 5 — Compute $RS_1$.**
$PS_1 = BS_1$ exactly, so there are no polynomials outside $BS_1$ to reduce.
$$RS_1 = \varnothing$$

**Termination.** $RS_1 = \varnothing$, so:
$$CS = \{2x^2 - 1,\ x - y\}$$

**Solution:**
From $2x^2 - 1 = 0$: $x = \pm\frac{1}{\sqrt{2}} = \pm\frac{\sqrt{2}}{2}$.
From $x - y = 0$: $y = x$.
Intersection points: $\left(\frac{\sqrt{2}}{2}, \frac{\sqrt{2}}{2}\right)$ and $\left(-\frac{\sqrt{2}}{2}, -\frac{\sqrt{2}}{2}\right)$ — the unit circle and line $x = y$ at $45°$ and $225°$.

---

## Part VIII: Testing Strategy

### 8.1 Unit Tests for Core Operations

```haskell
-- Property: prem(p, f) is reduced wrt f
prop_premReduced :: Polynomial -> Polynomial -> Property
prop_premReduced g f =
  cls f > 0 ==> isReducedWrt (pseudoRem g f) f

-- Property: the remainder formula holds
prop_premFormula :: Polynomial -> Polynomial -> Property
prop_premFormula g f =
  cls f > 0 ==>
    let r = pseudoRem g f
        i = ini f
        -- Check: exists s,q such that i^s * g = q * f + r
        -- (Verify by checking degrees and leading terms)
    in cls r < cls f || ldeg r < ldeg f

-- Property: charset terminates and satisfies (c1)
prop_charsetTerminates :: [Polynomial] -> Property
prop_charsetTerminates ps =
  not (null ps) ==>
    let AscendingSet cs = characteristicSet ps
    in all (\p -> isZero (iteratedPrem p cs)) ps

-- Property: well-ordering (each new BS is lower than previous)
prop_wellOrdering :: [Polynomial] -> Property
prop_wellOrdering ps = ...  -- Track BS sequence and verify strict descent
```

### 8.2 Integration Tests: Known Theorems

| Theorem | Variables | Expected result |
|---|---|---|
| Collinear midpoints (Midpoint theorem) | 6 | `prem = 0` |
| Pythagoras | 6 | `prem = 0` |
| Pappus' theorem | 8 | `prem = 0` |
| Desargues' theorem | 10 | `prem = 0` |
| Simson's theorem | 8 | `prem = 0` |
| False statement (conclusion wrong) | any | `prem ≠ 0` |

### 8.3 Regression on Classic Examples

- **Heron's formula derivation**: verify that $CS$ of the triangle-area system yields $C_1 = 2a^2b^2 + 2a^2c^2 + 2b^2c^2 - a^4 - b^4 - c^4 + 16S^2$.
- **Unit circle ∩ line**: verify $CS = \{2x^2-1, x-y\}$ as above.
- **Circle ∩ parabola**: verify pseudo-remainder factorisation.

---

## Summary: Key Invariants to Maintain

1. **Variable ordering is fixed and global.** Every function receives the same ordering context.
2. **Polynomials are always exact.** Use rational arithmetic; never use floating point.
3. **Zero polynomials are always discarded immediately.** Never add $0$ to any set.
4. **The remainder set $RS$ drives progress.** If $RS = \varnothing$, the algorithm terminates.
5. **The basic set ordering strictly decreases.** This is the termination argument; verify it holds in any modified variant.
6. **Non-degeneracy conditions = initials of $CS$.** Report them; the theorem is proved modulo these conditions.
7. **`prem(C, CS) = 0` is necessary, not always sufficient.** For simple or irreducible systems, it is also sufficient (Theorem 2.16). For general triangular sets, sufficiency requires a power: $\text{prem}(C^d, TS) = 0$ for some $d$.
