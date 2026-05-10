import DependencyAnalysis.Inference

/-!
# `isAuxiliaryName` smoke tests

Locks in the behaviour of `DependencyAnalysis.isAuxiliaryName`
across three layers:

1. exact-match base names from `auxiliarySuffixComponents`,
2. `match_<digits>` and `<base>_<digits>` numeric variants on the
   last component,
3. walk-up through `go` / `eq` / `eq_<digits>` / `eq_def`
   sub-helpers to find an aux ancestor.

The third layer was added after the L4YAML.FGM Phase 4 case
study found that downstream `#ifg` metrics were dominated by
`<inductive>.brecOn_N → <…>.brecOn_N.go` aux→aux edges. The
must-not-filter section pins the discrimination from user
identifiers like `Foo.go`, `Foo.bar.go`, `Foo.below.helper`,
or `Foo.brecOn_1.helper`.

Run with:

```
lake build DependencyAnalysisTests
```
-/

namespace DependencyAnalysis.Tests

open Lean DependencyAnalysis

/-! ## Layer 1: exact-match base components (already filtered pre-Phase-4) -/

/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.rec
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.recOn
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.casesOn
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.brecOn
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.below
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.injEq
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.noConfusion
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.match_1
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.match_42

/-! ## Layer 2: numbered base variants (`<base>_<digits>`) -/

/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.brecOn_1
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.brecOn_42
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.below_2
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.recOn_3
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.casesOn_1

/-! ## Layer 3: sub-helper walk-up (`.go`, `.eq`, `.eq_<digits>`) -/

/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.brecOn.go
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.brecOn.eq
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.brecOn_1.go
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.brecOn_5.eq
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.below_1.go
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.brecOn_1.eq_2
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.brecOn.eq_def
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.recOn.eq
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.match_1.go

-- Multi-step walk: chain through two sub-helpers to reach the aux.
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.brecOn_1.go.eq

-- Standalone equation-lemma suffixes on a user function (`eq_def`,
-- `eq_unfold`, `eq_<digits>`) — Lean elaborator emits these for every
-- non-trivial `def`; they are not authored, so they're filtered.
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.eq_def
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.eq_unfold
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.eq_1
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.eq_42

/-! ## Must NOT filter — discrimination tests -/

-- User function happens to be called `go`/`eq` — last component is a
-- sub-helper but the parent is not an aux.
/-- info: false -/
#guard_msgs in #eval isAuxiliaryName `Foo.go
/-- info: false -/
#guard_msgs in #eval isAuxiliaryName `Foo.eq
/-- info: false -/
#guard_msgs in #eval isAuxiliaryName `Foo.bar.go
/-- info: false -/
#guard_msgs in #eval isAuxiliaryName `Foo.bar.eq

-- User identifiers that look superficially like an aux but don't match.
/-- info: false -/
#guard_msgs in #eval isAuxiliaryName `Foo.brecon
/-- info: false -/
#guard_msgs in #eval isAuxiliaryName `Foo.brecOn_
/-- info: false -/
#guard_msgs in #eval isAuxiliaryName `Foo.brecOn_x
/-- info: false -/
#guard_msgs in #eval isAuxiliaryName `Foo.brecOnHelper
/-- info: false -/
#guard_msgs in #eval isAuxiliaryName `Foo.match_x

-- A component matching an aux base in the *middle* of the chain, with
-- an unrecognised tail, must not trip the walk: the walk only continues
-- through recognised sub-helpers.
/-- info: false -/
#guard_msgs in #eval isAuxiliaryName `Foo.below.helper
/-- info: false -/
#guard_msgs in #eval isAuxiliaryName `Foo.brecOn_1.helper

-- Pre-existing Phase-3.7 regression cases (component-boundary match,
-- not substring): user names ending in "rec" or starting with the
-- aux base must NOT be filtered.
/-- info: false -/
#guard_msgs in #eval isAuxiliaryName `Foo.g_listrec
/-- info: false -/
#guard_msgs in #eval isAuxiliaryName `Foo.myrec
/-- info: false -/
#guard_msgs in #eval isAuxiliaryName `Foo.vec

/-! ## Policy customization — `addBases` / `addMatcher` -/

-- Client adds `myaux` as a new base. After extension, `Foo.myaux`
-- becomes auxiliary while the default policy still says false.
private def policyWithMyaux : AuxiliaryNamePolicy :=
  defaultPolicy.addBases ["myaux"]

/-- info: false -/
#guard_msgs in #eval isAuxiliaryName `Foo.myaux defaultPolicy
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.myaux policyWithMyaux

-- Numbered + sub-helper walk through a custom base.
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.myaux.go policyWithMyaux

-- `addMatcher` accepts any `String → Bool`. Client filters names ending
-- in "Helper".
private def policyEndsHelper : AuxiliaryNamePolicy :=
  defaultPolicy.addMatcher (·.endsWith "Helper")

/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.someHelper policyEndsHelper
/-- info: false -/
#guard_msgs in #eval isAuxiliaryName `Foo.someHelper defaultPolicy

/-! ## Policy customization — `removeBases` / `removeMatcher` -/

-- `removeBases` lets a client subtract a default base. With
-- `removeBases ["recOn"]`, `Foo.recOn` is no longer filtered, but
-- other defaults survive.
private def policyKeepRecOn : AuxiliaryNamePolicy :=
  defaultPolicy.removeBases ["recOn"]

/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.recOn defaultPolicy
/-- info: false -/
#guard_msgs in #eval isAuxiliaryName `Foo.recOn policyKeepRecOn
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.brecOn policyKeepRecOn
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.injEq policyKeepRecOn

-- `removeMatcher` drops a *predicate* (any matcher that accepts the
-- given samples). Removing the equation-lemma matcher unfilters
-- `Foo.eq_def` / `Foo.eq_1` / `Foo.eq_unfold` while leaving the rest
-- of the policy intact.
private def policyKeepEqns : AuxiliaryNamePolicy :=
  defaultPolicy.removeMatcher ["eq_def"]

/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.eq_def defaultPolicy
/-- info: false -/
#guard_msgs in #eval isAuxiliaryName `Foo.eq_def policyKeepEqns
/-- info: false -/
#guard_msgs in #eval isAuxiliaryName `Foo.eq_1 policyKeepEqns
/-- info: true -/
#guard_msgs in #eval isAuxiliaryName `Foo.brecOn policyKeepEqns

/-! ## `shouldFilter` honours the policy argument -/

/-- info: true -/
#guard_msgs in #eval shouldFilter `Foo.brecOn
/-- info: false -/
#guard_msgs in #eval shouldFilter `Foo.brecOn (defaultPolicy.removeBases ["brecOn"])
-- Still filtered because `filterNames` covers logical-connective `And`.
/-- info: true -/
#guard_msgs in #eval shouldFilter ``And

end DependencyAnalysis.Tests
