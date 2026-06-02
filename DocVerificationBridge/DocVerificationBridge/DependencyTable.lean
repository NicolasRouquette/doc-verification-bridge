import Lean
import DependencyAnalysis.Types

/-!
# Theorem dependency table

Reusable analysis over a populated `NameMap APIMeta` that produces a
per-declaration table of forward dependencies (`dependsOn` from the proof
term) and inverse callers (transposed from the global dependency graph).
The primary use case is identifying *deletion candidates* — declarations
that carry `sorry` and have no consumers within the analyzed scope.

The classification pipeline (`Classify.classifyAllDeclarations`) already
populates `APIMeta.dependsOn` and `APIMeta.hasSorry`; this module just
filters and transposes.

Edges to/from declarations outside `allEntries` (e.g. into `Lean.*` or
unclassified packages) are dropped — the table reflects coupling within
the analyzed scope.  Re-classify with broader module coverage if you need
external-edge accuracy.
-/

namespace DocVerificationBridge.DependencyTable

open Lean DependencyAnalysis

/-- One row of the dependency table. -/
structure Entry where
  name      : Name
  module    : Name
  isTheorem : Bool
  hasSorry  : Bool
  /-- In-scope forward dependencies (proof-term references that are also
      classified declarations in `allEntries`). -/
  dependsOn : Array Name
  /-- In-scope callers.  When empty AND `hasSorry`, the declaration is a
      deletion candidate within the analyzed scope. -/
  calledBy  : Array Name
deriving Inhabited

/-- A dependency table restricted to declarations matching some
    `namespacePrefix`. -/
structure Table where
  entries         : Array Entry
  namespacePrefix : Name
  /-- Whether `calledBy` was filtered to exclude callers inside the
      namespace prefix.  Setting this to `true` surfaces transitively
      dead clusters: an entry whose only callers are themselves inside
      the prefix is now treated as having "no callers" for the purpose
      of `deletionCandidates`. -/
  externalOnly    : Bool
deriving Inhabited

/-- Declarations with `sorry` and no in-scope callers.  When the table
    was built with `externalOnly := true`, this lifts to "no callers
    outside the namespace" — i.e. captures whole transitively-dead
    clusters, not just leaves of the call graph. -/
def Table.deletionCandidates (t : Table) : Array Entry :=
  t.entries.filter fun e => e.hasSorry && e.calledBy.isEmpty

/-- All declarations with no in-scope callers, regardless of `sorry`
    status.  Superset of `deletionCandidates` — also includes dead
    non-sorry'd theorems and helper definitions.  Useful for whole-file
    cleanups where the goal is to remove unreferenced machinery, not
    just retire failing proofs. -/
def Table.deadDeclarations (t : Table) : Array Entry :=
  t.entries.filter fun e => e.calledBy.isEmpty

/-- Build a dependency table over `allEntries`, restricted to
    declarations whose name has `namespacePrefix` as a structural prefix.

The inverse map (`calledBy`) is computed over the *entire* `allEntries`
universe before filtering — so a row whose `calledBy.isEmpty` really has
no consumers anywhere in `allEntries`, not just inside the prefix.  This
is what makes the deletion-candidate analysis honest.

When `externalOnly := true`, callers from inside `namespacePrefix` are
suppressed.  This is the right setting for "is this cluster dead?"
questions — a closed loop of mutually-referencing theorems within a
namespace will all show empty `calledBy` and surface as deletion
candidates together. -/
def build (allEntries : NameMap APIMeta) (namespacePrefix : Name)
    (externalOnly : Bool := false) : Table := Id.run do
  -- Restrict edge endpoints to in-scope (classified) names; this drops
  -- noise from `Lean.*`, `Std.*`, etc. that would otherwise inflate the
  -- table without adding signal.
  let mut inScope : NameSet := {}
  for (n, _) in allEntries do
    inScope := inScope.insert n

  -- Global inverse map: for every classified declaration, who depends on
  -- it?  Built once over the whole universe.  When externalOnly is set,
  -- callers from inside the namespace are excluded — so the resulting
  -- map answers "who outside this namespace depends on n?".
  let mut callers : NameMap (Array Name) := {}
  for (n, m) in allEntries do
    let callerIsInternal := namespacePrefix.isPrefixOf n
    unless externalOnly && callerIsInternal do
      for dep in m.dependsOn do
        if inScope.contains dep then
          let cur := (callers.find? dep).getD #[]
          callers := callers.insert dep (cur.push n)

  -- Entries: only the prefix-matching ones, but with edges populated
  -- from the global maps.
  let mut entries : Array Entry := #[]
  for (n, m) in allEntries do
    if namespacePrefix.isPrefixOf n then
      let depsInScope := m.dependsOn.filter (inScope.contains ·)
      let callersOf := (callers.find? n).getD #[]
      entries := entries.push {
        name := n, module := m.module,
        isTheorem := m.isTheorem, hasSorry := m.hasSorry,
        dependsOn := depsInScope, calledBy := callersOf
      }
  -- Deterministic ordering for diff-friendly output.
  let sorted := entries.qsort fun a b => a.name.toString < b.name.toString
  return { entries := sorted, namespacePrefix, externalOnly }

/-- Render the table as Markdown.  The "Deletion candidates" section at
    the bottom is the actionable output — it lists every row that has
    `sorry` and an empty `calledBy`. -/
def Table.toMarkdown (t : Table) : String := Id.run do
  let total := t.entries.size
  let withSorry := (t.entries.filter (·.hasSorry)).size
  let dead := t.deletionCandidates.size
  let modeNote := if t.externalOnly
    then s!"\nMode: **external-only** — callers inside `{t.namespacePrefix}` are suppressed, so transitively-dead clusters surface as deletion candidates.\n\n"
    else ""
  let mut out := s!"# Dependency table for `{t.namespacePrefix}`\n\n"
  out := out ++ s!"{total} declarations · {withSorry} with `sorry` · {dead} deletion candidate(s).{modeNote}\n"
  out := out ++ "| Declaration | Kind | Sorry | # Callers | # Deps |\n"
  out := out ++ "|---|---|:---:|---:|---:|\n"
  for e in t.entries do
    let s := if e.hasSorry then "⚠️" else "—"
    let kind := if e.isTheorem then "thm" else "def"
    out := out ++ s!"| `{e.name}` | {kind} | {s} | {e.calledBy.size} | {e.dependsOn.size} |\n"
  out := out ++ "\n## Deletion candidates (sorry'd ∧ no callers)\n\n"
  out := out ++ "Sorry'd declarations with no in-scope callers.  Highest-signal cleanup target — removing these directly reduces the sorry count.\n\n"
  let candidates := t.deletionCandidates
  if candidates.isEmpty then
    out := out ++ "_(none — every sorry'd declaration has at least one in-scope consumer)_\n"
  else
    for e in candidates do
      out := out ++ s!"- `{e.name}` (in `{e.module}`)\n"
  -- Broader dead-code list: every entry with no callers, sorry'd or
  -- not.  Surfaces helper machinery that became unreferenced after a
  -- refactor and is now technical debt.
  let dead := t.deadDeclarations
  let deadNonSorry := dead.filter (fun e => !e.hasSorry)
  if !deadNonSorry.isEmpty then
    out := out ++ s!"\n## Dead non-sorry declarations ({deadNonSorry.size})\n\n"
    out := out ++ "Theorems and defs with no in-scope callers that are *not* sorry'd.  Often abandoned helper lemmas, intermediate steps from earlier proof attempts, or auxiliary structures whose consumer was removed.  Safe to delete in the same surgical pass as the deletion candidates above.\n\n"
    for e in deadNonSorry do
      out := out ++ s!"- `{e.name}` (in `{e.module}`)\n"
  out := out ++ s!"\n## Total dead surface\n\n{dead.size} declarations have no in-scope callers ({t.deletionCandidates.size} sorry'd + {deadNonSorry.size} non-sorry'd).\n"
  -- Per-entry caller detail (for the rows that have callers, expanded for
  -- triage).
  let withCallers := t.entries.filter (·.calledBy.size > 0)
  if !withCallers.isEmpty then
    out := out ++ "\n## Caller detail (in-scope edges)\n\n"
    for e in withCallers do
      out := out ++ s!"### `{e.name}`\n\n"
      for c in e.calledBy do
        out := out ++ s!"- `{c}`\n"
      out := out ++ "\n"
  return out

/-- JSON dump (one object per entry).  Useful for piping to `jq` or
    feeding downstream tools. -/
def Table.toJson (t : Table) : Json :=
  let entries : Array Json := t.entries.map fun e =>
    Json.mkObj [
      ("name",      Json.str e.name.toString),
      ("module",    Json.str e.module.toString),
      ("isTheorem", Json.bool e.isTheorem),
      ("hasSorry",  Json.bool e.hasSorry),
      ("dependsOn", Json.arr (e.dependsOn.map (fun n => Json.str n.toString))),
      ("calledBy",  Json.arr (e.calledBy.map (fun n => Json.str n.toString)))
    ]
  Json.mkObj [
    ("namespacePrefix", Json.str t.namespacePrefix.toString),
    ("externalOnly",   Json.bool t.externalOnly),
    ("totalEntries",   Json.num t.entries.size),
    ("entries",        Json.arr entries)
  ]

end DocVerificationBridge.DependencyTable
