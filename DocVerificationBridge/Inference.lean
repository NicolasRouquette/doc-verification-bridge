-- DocVerificationBridge/Inference.lean
-- Automatic inference of assumes/proves/validates from theorem types

import Lean
import DocVerificationBridge.Types
import DocVerificationBridge.Compatibility

/-!
# Automatic Inference of Theorem Annotations

This module provides utilities to automatically infer the `assumes`, `proves`, and
`validates` fields for theorems by analyzing their type structure.

## Algorithm

Given a theorem type like:
```
∀ (g : Graph) (h₁ : IsAcyclic g) (h₂ : IsConnected g), NoSelfLoop g ∧ IsTree g
```

We decompose it using `forallTelescope` and:

1. **Hypotheses (assumes candidates)**: Collect Prop-typed parameters whose head
   constant is a known definition. Example: `IsAcyclic`, `IsConnected`

2. **Conclusion (proves candidates)**: Extract head constants from the conclusion.
   For compound conclusions (`∧`, `∨`, `↔`), we recursively extract all head constants.
   Example: `NoSelfLoop`, `IsTree`

3. **Validates candidates**: Extract Bool-returning functions from `f x = true/false`
   patterns, which indicate bridging theorems between Bool and Prop.
-/

namespace DocVerificationBridge

open Lean Meta

/-!
## Filtering and Classification
-/

/-- Names that should be filtered out (logical connectives, basic types) -/
def filterNames : Array Name :=
  #[``And, ``Or, ``Not, ``Iff, ``Eq, ``HEq, ``Ne, ``True, ``False,
    ``Exists, ``Subtype, ``PLift, ``ULift, ``PUnit, ``Unit,
    ``Decidable, ``DecidableEq, ``DecidablePred, ``DecidableRel,
    ``BEq, ``Hashable, ``Ord, ``LT, ``LE, ``Inhabited, ``Nonempty]

/-- Suffixes that indicate compiler-generated auxiliary definitions -/
def auxiliarySuffixes : List String :=
  [".match_", ".rec", ".recOn", ".casesOn", ".brecOn", ".below", ".ndrec", ".ndrecOn",
   ".noConfusion", ".noConfusionType", ".sizeOf", ".sizeOf_spec", ".injEq", ".inj", ".mk"]

/-- Check if a name is a compiler-generated auxiliary -/
def isAuxiliaryName (n : Name) : Bool :=
  let str := n.toString
  auxiliarySuffixes.any fun suffix =>
    let suffixNoLeadingDot := suffix.dropWhile (· == '.')
    if str.endsWith suffixNoLeadingDot then true
    else
      let parts := str.splitOn suffix
      if parts.length <= 1 then false
      else parts.drop 1 |>.all fun part =>
        match part.toList with
        | [] => true
        | c :: _ => c.isDigit || c == '.'

/-- Check if a name should be filtered out -/
def shouldFilter (n : Name) : Bool :=
  filterNames.contains n || n.isInternal || isAuxiliaryName n

/-- Get the root namespace (first component) from a declaration name -/
def getRootNamespace (declName : Name) : Name :=
  match declName with
  | .str .anonymous s => .str .anonymous s
  | .num .anonymous n => .num .anonymous n
  | .str p _ => getRootNamespace p
  | .num p _ => getRootNamespace p
  | .anonymous => .anonymous

/-- Get all prefixes considered "internal" for a declaration.
    Includes both the root namespace of the declaration and the module name. -/
def getInternalPrefixes (env : Environment) (declName : Name) : Array String :=
  let root := getRootNamespace declName
  let rootStr := root.toString
  -- Also get the module prefix if available
  let modPfx := match env.getModuleIdxFor? declName with
    | some modIdx =>
      let modName := env.header.moduleNames[modIdx.toNat]!
      let modRoot := getRootNamespace modName
      modRoot.toString
    | none => ""
  -- Combine both prefixes (dedupe if same)
  let prefixes := if rootStr.isEmpty then #[]
    else if modPfx.isEmpty || modPfx == rootStr then #[rootStr]
    else #[rootStr, modPfx]
  prefixes

/-- Check if a name is from the same project as the theorem being analyzed -/
def isInternalName (env : Environment) (internalPrefixes : Array String) (n : Name) : Bool :=
  if internalPrefixes.isEmpty then false
  else
    match env.getModuleIdxFor? n with
    | some modIdx =>
      let modName := env.header.moduleNames[modIdx.toNat]!
      let modStr := modName.toString
      internalPrefixes.any fun pfx => modStr.startsWith pfx
    | none =>
      let nStr := n.toString
      internalPrefixes.any fun pfx => nStr.startsWith pfx

/-- Extract head constant name from an expression -/
def getHeadConstName? (e : Expr) : Option Name :=
  match e.getAppFn with
  | .const name _ => if shouldFilter name then none else some name
  | _ => none

/-- Check if a name refers to a type declaration (inductive, structure, class) -/
def isTypeDeclaration (env : Environment) (n : Name) : Bool :=
  match env.find? n with
  | some (.inductInfo _) => true
  | some (.ctorInfo _) => true
  | some (.recInfo _) => true
  | some (.defnInfo info) => info.type.isSort || info.type.isType
  | some (.axiomInfo info) => info.type.isSort || info.type.isType
  | _ => false

/-!
## Inference Results
-/

/-- Information about an inferred name -/
structure InferredName where
  name : Name
  source : String
  isInternal : Bool
deriving Repr, Inhabited

/-- Result of inferring assumes/proves/validates from a theorem type -/
structure InferredTheoremAnnotations where
  assumesCandidates : Array InferredName := #[]
  provesCandidates : Array InferredName := #[]
  validatesCandidates : Array InferredName := #[]
  /-- Names of theorems/lemmas this proof depends on -/
  dependsOnCandidates : Array InferredName := #[]
  internalHypothesisTypes : Array InferredName := #[]
  externalConclusionTypes : Array InferredName := #[]
  externalHypothesisTypes : Array InferredName := #[]
  internalConclusionTypes : Array InferredName := #[]
  internalConclusionHeads : Array InferredName := #[]
  notes : Array String := #[]
deriving Repr, Inhabited

/-!
## Core Inference Logic
-/

/-- Cache for whnf results on type expressions.
    Used to avoid redundant whnf calls on the same type within a declaration. -/
abbrev WhnfCache := IO.Ref (Std.HashMap Expr Expr)

/-- Create a new whnf cache -/
def WhnfCache.new : IO WhnfCache := IO.mkRef {}

/-- Cached whnf for type expressions.
    Use this for whnf calls on types (which have high sharing).
    Don't use for whnf calls during expression traversal (which vary per call). -/
def whnfCached (cache : WhnfCache) (e : Expr) : MetaM Expr := do
  if let some result := (← cache.get).get? e then
    return result
  let result ← whnf e
  cache.modify (·.insert e result)
  return result

/-- Check if an expression is a type class constraint -/
def isTypeClassConstraint (cache : WhnfCache) (e : Expr) : MetaM Bool := do
  let e ← whnfCached cache e
  match e.getAppFn with
  | .const name _ =>
    let env ← getEnv
    return isClass env name
  | _ => return false

/-- Pretty print a type for source description -/
def ppTypeShort (e : Expr) : MetaM String := do
  let fmt ← ppExpr e
  let str := fmt.pretty
  if str.length > 60 then
    -- Use takeCompat for cross-version compatibility (String.ofList doesn't exist in Lean < 4.26)
    return str.takeCompat 57 ++ "..."
  else
    return str

mutual
/-- Recursively collect head constants from a Prop expression -/
partial def collectHeadConstants (e : Expr) (sourceDesc : String := "") (internalPrefixes : Array String := #[]) : MetaM (Array (Name × String)) := do
  let headName := getHeadConstName? e

  if let some p := e.app1? ``Not then
    return ← collectHeadConstants p sourceDesc internalPrefixes

  let e' ← if headName.isNone then whnf e else pure e

  if let some (p, q) := e'.app2? ``And then
    let left ← collectHeadConstants p sourceDesc internalPrefixes
    let right ← collectHeadConstants q sourceDesc internalPrefixes
    return left ++ right
  else if let some (p, q) := e'.app2? ``Or then
    let left ← collectHeadConstants p sourceDesc internalPrefixes
    let right ← collectHeadConstants q sourceDesc internalPrefixes
    return left ++ right
  else if let some (p, q) := e'.app2? ``Iff then
    let left ← collectHeadConstants p sourceDesc internalPrefixes
    let right ← collectHeadConstants q sourceDesc internalPrefixes
    return left ++ right
  else if e'.isForall then
    forallTelescope e' fun _ body => collectHeadConstants body sourceDesc internalPrefixes
  else
    match e'.getAppFn, e'.getAppArgs with
    | .const ``Exists _, #[_, body] =>
      lambdaTelescope body fun _ innerBody => collectHeadConstants innerBody sourceDesc internalPrefixes
    | .const ``Eq _, #[_, lhs, rhs] =>
      let leftNames ← collectHeadConstantsFromTerm lhs sourceDesc
      let rightNames ← collectHeadConstantsFromTerm rhs sourceDesc
      return leftNames ++ rightNames
    | _, _ =>
      let mut result : Array (Name × String) := #[]
      if let some name := headName then
        result := result.push (name, sourceDesc)
      for arg in e'.getAppArgs do
        let termNames ← collectHeadConstantsFromTerm arg sourceDesc
        result := result ++ termNames
      return result

/-- Extract head constants from term expressions -/
partial def collectHeadConstantsFromTerm (e : Expr) (sourceDesc : String := "") : MetaM (Array (Name × String)) := do
  let mut result : Array (Name × String) := #[]

  if e.isLambda then
    result := result ++ (← lambdaTelescope e fun _ body => collectHeadConstantsFromTerm body sourceDesc)
    return result

  match e.getAppFn with
  | .const name _ =>
    unless shouldFilter name do
      result := result.push (name, sourceDesc)
  | _ => pure ()

  for arg in e.getAppArgs do
    result := result ++ (← collectHeadConstantsFromTerm arg sourceDesc)

  return result
end

/-- Extract Bool-returning function from `f x = true` or `f x = false` pattern -/
def extractBoolFunction (cache : WhnfCache) (e : Expr) : MetaM (Option Name) := do
  match e.getAppFn, e.getAppArgs with
  | .const ``Eq _, #[_, lhs, rhs] =>
    let isTrueOrFalse := match rhs with
      | .const ``Bool.true _ => true
      | .const ``Bool.false _ => true
      | _ => false
    if isTrueOrFalse then
      match lhs.getAppFn with
      | .const name _ =>
        let lhsType ← try inferType lhs catch _ => pure (mkConst ``Bool)
        -- Use cached whnf since lhsType is a type expression (high sharing potential)
        let lhsType ← whnfCached cache lhsType
        if lhsType.isConstOf ``Bool then return some name
        else return none
      | _ => return none
    else return none
  | _, _ => return none

/-- Collect Bool-returning functions from bridging patterns -/
def collectBoolFunctionsShallow (cache : WhnfCache) (e : Expr) (sourceDesc : String := "") : MetaM (Array (Name × String)) := do
  let mut result : Array (Name × String) := #[]

  if let some name ← extractBoolFunction cache e then
    unless shouldFilter name do
      result := result.push (name, sourceDesc)

  if let some (p, q) := e.app2? ``Iff then
    let iffSourceDesc := s!"{sourceDesc} (Iff)"
    if let some name ← extractBoolFunction cache p then
      unless shouldFilter name do
        result := result.push (name, iffSourceDesc)
    if let some name ← extractBoolFunction cache q then
      unless shouldFilter name do
        result := result.push (name, iffSourceDesc)

  if let some (p, q) := e.app2? ``And then
    if let some name ← extractBoolFunction cache p then
      unless shouldFilter name do
        result := result.push (name, sourceDesc)
    if let some name ← extractBoolFunction cache q then
      unless shouldFilter name do
        result := result.push (name, sourceDesc)

  return result

/-!
## Proof Dependency Extraction

Extract the names of theorems/lemmas that a proof term directly uses.
This gives us the "depends on" relationship: if theorem A uses theorem B
in its proof, then A depends on B.

**Performance Note**: We use Lean's built-in `Expr.foldConsts` which uses
pointer-based DAG traversal (`PtrSet Expr`). This is critical because proof
terms use structural sharing - the same subexpression can appear many times.
A naive tree traversal would visit shared nodes exponentially many times,
but `foldConsts` visits each unique expression pointer exactly once.

For example, `distDeriv_commute` has a proof term that as a tree has billions
of nodes, but as a DAG (with sharing) has only ~1M unique nodes. Without
proper DAG traversal, it took 8+ hours; with it, it takes seconds.

**Implementation Note**: `Expr.foldConsts` is implemented in `Lean.Util.FoldConsts`
using unsafe pointer hashing for efficiency. It's the same approach used by
Lean's compiler and type checker for traversing expressions.
-/

/-- Collect all constant names referenced in an expression (proof term).
    Uses Lean's built-in DAG-aware foldConsts which handles structural sharing. -/
def collectProofDependencies (env : Environment) (e : Expr) : Array Name :=
  -- Get all constants using Lean's efficient DAG traversal
  let allConsts := e.foldConsts #[] fun name acc => acc.push name
  -- Filter to just theorems/axioms
  allConsts.filter fun name =>
    if shouldFilter name then false
    else match env.find? name with
      | some (.thmInfo _) => true
      | some (.axiomInfo _) => true
      | _ => false

/-- Collect proof dependencies with a node limit.
    Since we now use proper DAG traversal via foldConsts, this should rarely
    hit any limit. The limit parameter is kept for API compatibility but
    no longer actively enforced (foldConsts handles it internally). -/
def collectProofDependenciesWithLimit (env : Environment) (e : Expr) (_nodeLimit : Nat := 10_000_000)
    : Array Name × Bool :=
  -- Using foldConsts means we traverse the DAG efficiently
  -- No need for manual node counting - foldConsts handles this properly
  (collectProofDependencies env e, false)

/-- Extract proof dependencies for a theorem, filtering to internal names only -/
def extractProofDependencies (env : Environment) (declName : Name) (internalPrefixes : Array String)
    : Array InferredName := Id.run do
  let some constInfo := env.find? declName | return #[]

  -- Get the proof term (value) if available
  let proofExpr := match constInfo with
    | .thmInfo info => some info.value
    | _ => none

  let some proof := proofExpr | return #[]

  let allDeps := collectProofDependencies env proof
  let mut result : Array InferredName := #[]

  for dep in allDeps do
    -- Skip self-references
    if dep == declName then continue
    let isInt := isInternalName env internalPrefixes dep
    result := result.push {
      name := dep
      source := "proof term"
      isInternal := isInt
    }

  return result

/-!
## Main Inference Entry Point
-/

/-- Infer assumes/proves candidates from a theorem's type -/
def inferTheoremAnnotations (declName : Name) : MetaM InferredTheoremAnnotations := do
  let env ← getEnv
  let some constInfo := env.find? declName | return { notes := #[s!"Declaration `{declName}` not found"] }

  let internalPrefixes := getInternalPrefixes env declName
  let type := constInfo.type

  -- Create whnf cache for type expressions within this declaration
  let cache ← WhnfCache.new

  forallTelescope type fun params conclusion => do
    let mut assumesCandidates : Array InferredName := #[]
    let mut provesCandidates : Array InferredName := #[]
    let mut validatesCandidates : Array InferredName := #[]
    let mut seenNames : Array Name := #[]
    let mut seenValidates : Array Name := #[]

    -- Process each parameter (hypothesis)
    for param in params do
      let paramType ← inferType param
      -- Use cached whnf since paramType is a type expression (high sharing potential)
      let paramTypeWhnf ← whnfCached cache paramType

      if ← isTypeClassConstraint cache paramTypeWhnf then
        continue

      if ← isProp paramTypeWhnf then
        let _rawParamName := (← getLCtx).find? param.fvarId! |>.map (·.userName) |>.getD `_
        let typeStr ← ppTypeShort paramType
        let sourceDesc := s!"hypothesis: {typeStr}"

        let heads ← collectHeadConstants paramType sourceDesc internalPrefixes
        for (head, src) in heads do
          unless seenNames.contains head do
            seenNames := seenNames.push head
            let isInt := isInternalName env internalPrefixes head
            assumesCandidates := assumesCandidates.push {
              name := head, source := src, isInternal := isInt
            }

        let boolFuns ← collectBoolFunctionsShallow cache paramType sourceDesc
        for (head, src) in boolFuns do
          unless seenValidates.contains head do
            seenValidates := seenValidates.push head
            let isInt := isInternalName env internalPrefixes head
            validatesCandidates := validatesCandidates.push {
              name := head, source := src, isInternal := isInt
            }

    -- Process conclusion
    let concSourceDesc := "conclusion"
    let concHeads ← collectHeadConstants conclusion concSourceDesc internalPrefixes
    for (head, src) in concHeads do
      unless seenNames.contains head do
        seenNames := seenNames.push head
        let isInt := isInternalName env internalPrefixes head
        provesCandidates := provesCandidates.push {
          name := head, source := src, isInternal := isInt
        }

    let concBoolFuns ← collectBoolFunctionsShallow cache conclusion concSourceDesc
    for (head, src) in concBoolFuns do
      unless seenValidates.contains head do
        seenValidates := seenValidates.push head
        let isInt := isInternalName env internalPrefixes head
        validatesCandidates := validatesCandidates.push {
          name := head, source := src, isInternal := isInt
        }

    -- Extract proof dependencies (theorems used in the proof term)
    -- Skip if SKIP_PROOF_DEPS=1 (set via skip_proof_deps config option for large projects)
    -- Note: When using parallel classification, proof deps are extracted separately
    let skipProofDeps := (← IO.getEnv "SKIP_PROOF_DEPS").getD "" == "1"
    let dependsOnCandidates := if skipProofDeps then #[]
      else extractProofDependencies env declName internalPrefixes

    return {
      assumesCandidates
      provesCandidates
      validatesCandidates
      dependsOnCandidates
    }

/-- Infer assumes/proves candidates WITHOUT proof dependencies (fast, for Phase 1 of parallel processing) -/
def inferTheoremAnnotationsLight (declName : Name) : MetaM InferredTheoremAnnotations := do
  let env ← getEnv
  let some constInfo := env.find? declName | return { notes := #[s!"Declaration `{declName}` not found"] }

  let internalPrefixes := getInternalPrefixes env declName
  let type := constInfo.type

  -- Create whnf cache for type expressions within this declaration
  let cache ← WhnfCache.new

  forallTelescope type fun params conclusion => do
    let mut assumesCandidates : Array InferredName := #[]
    let mut provesCandidates : Array InferredName := #[]
    let mut validatesCandidates : Array InferredName := #[]
    let mut seenNames : Array Name := #[]
    let mut seenValidates : Array Name := #[]

    -- Process each parameter (hypothesis)
    for param in params do
      let paramType ← inferType param
      let paramTypeWhnf ← whnfCached cache paramType

      if ← isTypeClassConstraint cache paramTypeWhnf then
        continue

      if ← isProp paramTypeWhnf then
        let typeStr ← ppTypeShort paramType
        let sourceDesc := s!"hypothesis: {typeStr}"

        let heads ← collectHeadConstants paramType sourceDesc internalPrefixes
        for (head, src) in heads do
          unless seenNames.contains head do
            seenNames := seenNames.push head
            let isInt := isInternalName env internalPrefixes head
            assumesCandidates := assumesCandidates.push {
              name := head, source := src, isInternal := isInt
            }

        let boolFuns ← collectBoolFunctionsShallow cache paramType sourceDesc
        for (head, src) in boolFuns do
          unless seenValidates.contains head do
            seenValidates := seenValidates.push head
            let isInt := isInternalName env internalPrefixes head
            validatesCandidates := validatesCandidates.push {
              name := head, source := src, isInternal := isInt
            }

    -- Process conclusion
    let concSourceDesc := "conclusion"
    let concHeads ← collectHeadConstants conclusion concSourceDesc internalPrefixes
    for (head, src) in concHeads do
      unless seenNames.contains head do
        seenNames := seenNames.push head
        let isInt := isInternalName env internalPrefixes head
        provesCandidates := provesCandidates.push {
          name := head, source := src, isInternal := isInt
        }

    let concBoolFuns ← collectBoolFunctionsShallow cache conclusion concSourceDesc
    for (head, src) in concBoolFuns do
      unless seenValidates.contains head do
        seenValidates := seenValidates.push head
        let isInt := isInternalName env internalPrefixes head
        validatesCandidates := validatesCandidates.push {
          name := head, source := src, isInternal := isInt
        }

    -- Return WITHOUT proof dependencies (they'll be added in parallel Phase 2)
    return {
      assumesCandidates
      provesCandidates
      validatesCandidates
      dependsOnCandidates := #[]
    }

/-!
## Theorem Kind Suggestion
-/

/-- Check if `sub` is a substring of `s` -/
def containsSubstr (s sub : String) : Bool := Id.run do
  for i in [:s.length] do
    let slice := s.drop i |>.take sub.length
    if slice == sub then return true
  return false

/-- Suggest a TheoremKind based on inference patterns -/
def suggestTheoremKind (inferred : InferredTheoremAnnotations) : Option TheoremKind := do
  -- Check for bridging pattern (validates not empty with internal names)
  if !(inferred.validatesCandidates.filter (·.isInternal)).isEmpty then
    return .bridgingProperty
  -- Default: if we have proves candidates, suggest mathematicalProperty
  if !(inferred.provesCandidates.filter (·.isInternal)).isEmpty then
    return .mathematicalProperty
  none

/-- Infer the direction of a bridging property -/
def inferBridgingDirection (inferred : InferredTheoremAnnotations) : Option BridgingDirection := do
  let validates := inferred.validatesCandidates
  if validates.isEmpty then none
  else
    let hasIffPattern := validates.any fun v => containsSubstr v.source "Iff"
    let hasValidatesInHyp := validates.any fun v => containsSubstr v.source "hypothesis"
    let hasValidatesInConc := validates.any fun v => containsSubstr v.source "conclusion"

    if hasIffPattern then some .iff
    else if hasValidatesInHyp then some .sound
    else if hasValidatesInConc then some .complete
    else some .iff  -- Default to iff if unclear

end DocVerificationBridge
