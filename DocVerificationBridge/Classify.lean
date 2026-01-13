-- DocVerificationBridge/Classify.lean
-- Automatic classification of declarations using doc-gen4's infrastructure

import Lean
import DocGen4.Process.Analyze
import DocGen4.Process.DocInfo
import DocVerificationBridge.Types
import DocVerificationBridge.Inference

/-!
# Automatic Declaration Classification

This module provides automatic classification of declarations without requiring
source code annotations. It uses doc-gen4's analysis infrastructure to enumerate
all declarations, then applies heuristics to classify them according to the
Four-Category Ontology.

## Classification Strategy

1. **Types** (from InductiveInfo, StructureInfo, ClassInfo):
   - Prop-based → `mathematicalAbstraction`
   - Data-carrying → `computationalDatatype`

2. **Definitions** (from DefinitionInfo):
   - Returns Prop → `mathematicalDefinition`
   - Returns non-Prop → `computationalOperation`

3. **Theorems** (from TheoremInfo):
   - Use `inferTheoremAnnotations` to determine assumes/proves/validates
   - Classify based on pattern (bridging, soundness, completeness, etc.)
-/

namespace DocVerificationBridge

open Lean Meta
open DocGen4.Process

/-!
## Classification Helpers
-/

/-- Check if a type is Prop-based (for classifying mathematical vs computational types) -/
def isPropBasedType (_env : Environment) (info : InductiveVal) : Bool :=
  -- A type is "Prop-based" if it lives in Prop
  info.type.isProp

/-- Classify an inductive type -/
def classifyInductiveType (env : Environment) (info : InductiveVal) : TypeCategory :=
  if isPropBasedType env info then
    .mathematicalAbstraction
  else
    .computationalDatatype

/-- Check if a definition returns Prop -/
def returnsProp (type : Expr) : MetaM Bool := do
  forallTelescope type fun _ body => do
    let body ← whnf body
    return body.isProp

/-- Classify a definition based on its return type -/
def classifyDefinition (type : Expr) : MetaM DefCategory := do
  if ← returnsProp type then
    return .mathematicalDefinition
  else
    return .computationalOperation

/-- Check if an expression contains any sorry (sorryAx application) -/
def exprContainsSorry (e : Expr) : Bool :=
  Option.isSome <| e.find? fun sub => sub.isAppOf ``sorryAx

/-!
## Blacklist Filtering
-/

/-- Check if a declaration should be excluded from the report -/
def isBlackListed (env : Environment) (name : Name) : MetaM Bool := do
  -- Check for declaration ranges (no ranges = compiler-generated)
  match ← findDeclarationRanges? name with
  | some _ =>
    -- Has source location, check other criteria
    pure (name.isInternal)
    <||> (pure <| isAuxRecursor env name)
    <||> (pure <| isNoConfusion env name)
    <||> (pure <| name.isInternalDetail)
    <||> isRec name
    <||> isMatcher name
  | none => return true  -- No source location = exclude

/-- Additional filter for names we don't want in the report -/
def shouldExclude (name : Name) : Bool :=
  let str := name.toString
  -- Exclude private/internal definitions
  str.startsWith "_private" ||
  str.startsWith "_root_"
  -- Note: We no longer exclude "inst*" names - instances are tracked for sorry detection

/-!
## Main Classification
-/

/-- Result of classifying all declarations in a module -/
structure ClassificationResult where
  /-- Map from declaration name to its metadata -/
  entries : NameMap APIMeta
  /-- Warnings or notes from classification -/
  notes : Array String
deriving Inhabited

/-- Data needed for parallel proof dependency extraction -/
structure ProofDepTask where
  name : Name
  proofExpr : Expr
  internalPrefixes : Array String
deriving Inhabited

/-- Classify a single constant from the environment (Phase 1: without proof deps) -/
def classifyConstantLight (env : Environment) (name : Name) (cinfo : ConstantInfo)
    (internalPrefixes : Array String) : MetaM (Option (APIMeta × Option ProofDepTask)) := do
  -- Skip blacklisted declarations
  if ← isBlackListed env name then return none
  if shouldExclude name then return none
  -- Skip if not from internal modules
  if !isInternalName env internalPrefixes name then return none

  match cinfo with
  | .thmInfo info =>
    -- Theorem: infer annotations (light - no proof deps yet)
    let inferred ← inferTheoremAnnotationsLight name
    let theoremKind := suggestTheoremKind inferred
    let bridgingDir := if theoremKind == some .bridgingProperty
      then inferBridgingDirection inferred
      else none
    -- Check if the proof contains sorry
    let hasSorry := exprContainsSorry info.value
    let thmData : TheoremData := {
      kind := theoremKind
      bridgingDirection := bridgingDir
      assumes := inferred.assumesCandidates.filter (·.isInternal) |>.map (·.name)
      proves := inferred.provesCandidates.filter (·.isInternal) |>.map (·.name)
      validates := inferred.validatesCandidates.filter (·.isInternal) |>.map (·.name)
      dependsOn := #[]  -- Will be filled in Phase 2
      hasSorry := hasSorry
    }
    let apiMeta := { kind := .apiTheorem thmData, coverage := .unverified }
    -- Return task data for parallel proof dep extraction
    let task := ProofDepTask.mk name info.value internalPrefixes
    return some (apiMeta, some task)

  | .defnInfo info =>
    -- Definition: classify by return type and check for sorry
    let category ← classifyDefinition info.type
    let hasSorry := exprContainsSorry info.value
    let defData : DefData := { category, hasSorry }
    return some ({ kind := .apiDef defData, coverage := .unverified }, none)

  | .inductInfo info =>
    -- Inductive type: classify as mathematical or computational
    let category := classifyInductiveType env info
    return some ({ kind := .apiType category, coverage := .unverified }, none)

  | .axiomInfo _ =>
    -- Axiom: treat as mathematical abstraction
    return some ({ kind := .apiType .mathematicalAbstraction, coverage := .axiomDependent }, none)

  | .opaqueInfo info =>
    -- Opaque constant (includes noncomputable instances and definitions)
    let category ← classifyDefinition info.type
    let hasSorry := exprContainsSorry info.value
    let defData : DefData := { category, hasSorry }
    return some ({ kind := .apiDef defData, coverage := .unverified }, none)

  | _ => return none  -- Skip other kinds (constructors, recursors, etc.)

/-- Classify a single constant (non-parallel version, used when proof deps are skipped) -/
def classifyConstant (env : Environment) (name : Name) (cinfo : ConstantInfo)
    (internalPrefixes : Array String) : MetaM (Option APIMeta) := do
  -- Skip blacklisted declarations
  if ← isBlackListed env name then return none
  if shouldExclude name then return none
  -- Skip if not from internal modules
  if !isInternalName env internalPrefixes name then return none

  match cinfo with
  | .thmInfo info =>
    -- Theorem: infer annotations and check for sorry
    let inferred ← inferTheoremAnnotations name
    let theoremKind := suggestTheoremKind inferred
    let bridgingDir := if theoremKind == some .bridgingProperty
      then inferBridgingDirection inferred
      else none
    -- Check if the proof contains sorry
    let hasSorry := exprContainsSorry info.value
    let thmData : TheoremData := {
      kind := theoremKind
      bridgingDirection := bridgingDir
      assumes := inferred.assumesCandidates.filter (·.isInternal) |>.map (·.name)
      proves := inferred.provesCandidates.filter (·.isInternal) |>.map (·.name)
      validates := inferred.validatesCandidates.filter (·.isInternal) |>.map (·.name)
      dependsOn := inferred.dependsOnCandidates.filter (·.isInternal) |>.map (·.name)
      hasSorry := hasSorry
    }
    return some { kind := .apiTheorem thmData, coverage := .unverified }

  | .defnInfo info =>
    -- Definition: classify by return type and check for sorry
    let category ← classifyDefinition info.type
    let hasSorry := exprContainsSorry info.value
    let defData : DefData := { category, hasSorry }
    return some { kind := .apiDef defData, coverage := .unverified }

  | .inductInfo info =>
    -- Inductive type: classify as mathematical or computational
    let category := classifyInductiveType env info
    return some { kind := .apiType category, coverage := .unverified }

  | .axiomInfo _ =>
    -- Axiom: treat as mathematical abstraction
    return some { kind := .apiType .mathematicalAbstraction, coverage := .axiomDependent }

  | .opaqueInfo info =>
    -- Opaque constant (includes noncomputable instances and definitions)
    let category ← classifyDefinition info.type
    let hasSorry := exprContainsSorry info.value
    let defData : DefData := { category, hasSorry }
    return some { kind := .apiDef defData, coverage := .unverified }

  | _ => return none  -- Skip other kinds (constructors, recursors, etc.)

/-- Extract proof dependencies for a single task (pure, can run in parallel) -/
def extractProofDepsTask (env : Environment) (task : ProofDepTask) : Array Name :=
  let allDeps := collectProofDependencies env task.proofExpr
  allDeps.filter fun dep =>
    dep != task.name && isInternalName env task.internalPrefixes dep

/-- Merge proof dependencies into APIMeta -/
def mergeProofDeps (apiMeta : APIMeta) (deps : Array Name) : APIMeta :=
  match apiMeta.kind with
  | .apiTheorem thmData =>
    let newThmData := { thmData with dependsOn := deps }
    { apiMeta with kind := .apiTheorem newThmData }
  | _ => apiMeta

/-- Classify all declarations with parallel proof dependency extraction -/
def classifyAllDeclarationsParallel (env : Environment) (modulePrefix : Name) (numWorkers : Nat := 8)
    : MetaM ClassificationResult := do
  let internalPrefixes := #[modulePrefix.toString]
  let mut entries : NameMap APIMeta := {}
  let mut proofDepTasks : Array (Name × ProofDepTask) := #[]
  let mut notes : Array String := #[]

  -- Count total constants for progress reporting
  let relevantConsts := env.constants.fold (init := #[]) fun acc name cinfo =>
    match env.getModuleIdxFor? name with
    | some modIdx =>
      let modName := env.header.moduleNames[modIdx.toNat]!
      if modulePrefix.isPrefixOf modName then acc.push (name, cinfo) else acc
    | none => acc

  let total := relevantConsts.size
  IO.println s!"  [3/7] Phase 1: Classifying {total} declarations (MetaM, single-threaded)..."
  (← IO.getStdout).flush

  -- Phase 1: Sequential MetaM classification (collect proof dep tasks)
  let mut processed := 0
  let progressInterval := max 1 (total / 20)

  for (name, cinfo) in relevantConsts do
    processed := processed + 1
    if processed % progressInterval == 0 then
      let pct := (processed * 100) / total
      IO.print s!"\r  [3/7] Classifying: {processed}/{total} ({pct}%)    "
      (← IO.getStdout).flush

    if let some (apiMeta, taskOpt) ← classifyConstantLight env name cinfo internalPrefixes then
      entries := entries.insert name apiMeta
      if let some task := taskOpt then
        proofDepTasks := proofDepTasks.push (name, task)

  IO.println s!"\r  [3/7] Classification complete: {entries.size} declarations    "
  (← IO.getStdout).flush

  -- Phase 2: Parallel proof dependency extraction
  if proofDepTasks.isEmpty then
    IO.println s!"  [4/7] No proof dependencies to extract (skipped)"
    (← IO.getStdout).flush
  else
    IO.println s!"  [4/7] Phase 2: Extracting proof deps for {proofDepTasks.size} theorems ({numWorkers} parallel workers)..."
    (← IO.getStdout).flush

    -- Create tasks for parallel execution
    let taskChunks := proofDepTasks.toList.toArray
    let chunkSize := max 1 ((taskChunks.size + numWorkers - 1) / numWorkers)

    -- Process in parallel using IO.mapTasks
    let chunks : Array (Array (Name × ProofDepTask)) := Id.run do
      let mut result : Array (Array (Name × ProofDepTask)) := #[]
      let mut i := 0
      while i < taskChunks.size do
        let endIdx := min (i + chunkSize) taskChunks.size
        result := result.push (taskChunks.extract i endIdx)
        i := endIdx
      result

    -- Spawn worker tasks
    let workers ← chunks.mapM fun chunk => do
      IO.asTask (prio := .default) do
        let mut results : Array (Name × Array Name) := #[]
        for (name, task) in chunk do
          let deps := extractProofDepsTask env task
          results := results.push (name, deps)
        return results

    -- Wait for all workers and collect results
    let mut allResults : Array (Name × Array Name) := #[]
    for worker in workers do
      let result ← IO.wait worker
      match result with
      | .ok results => allResults := allResults ++ results
      | .error e => notes := notes.push s!"Worker error: {e}"

    -- Merge proof deps into entries
    for (name, deps) in allResults do
      if let some existing := entries.find? name then
        entries := entries.insert name (mergeProofDeps existing deps)

    IO.println s!"  [4/7] Proof deps complete: extracted for {allResults.size} theorems"
    (← IO.getStdout).flush

  return { entries, notes }

/-- Classify all declarations from relevant modules
    @param skipProofDeps If true, skip proof dependency extraction entirely (fast mode)
    @param proofDepWorkers Number of parallel workers for proof dep extraction (0 = sequential) -/
def classifyAllDeclarations (env : Environment) (modulePrefix : Name)
    (skipProofDeps : Bool := false) (proofDepWorkers : Nat := 0) : MetaM ClassificationResult := do
  let internalPrefixes := #[modulePrefix.toString]

  if skipProofDeps then
    -- Fast mode: skip proof deps entirely
    IO.println s!"  (--skip-proof-deps: skipping proof dependency extraction)"
    (← IO.getStdout).flush
    let mut entries : NameMap APIMeta := {}
    let mut notes : Array String := #[]

    let relevantConsts := env.constants.fold (init := #[]) fun acc name cinfo =>
      match env.getModuleIdxFor? name with
      | some modIdx =>
        let modName := env.header.moduleNames[modIdx.toNat]!
        if modulePrefix.isPrefixOf modName then acc.push (name, cinfo) else acc
      | none => acc

    let total := relevantConsts.size
    IO.println s!"  [3/7] Classifying {total} declarations (no proof deps)..."
    (← IO.getStdout).flush

    let mut processed := 0
    let progressInterval := max 1 (total / 20)

    for (name, cinfo) in relevantConsts do
      processed := processed + 1
      if processed % progressInterval == 0 then
        let pct := (processed * 100) / total
        IO.print s!"\r  [3/7] Classifying: {processed}/{total} ({pct}%)    "
        (← IO.getStdout).flush

      if let some (apiMeta, _) ← classifyConstantLight env name cinfo internalPrefixes then
        entries := entries.insert name apiMeta

    IO.println s!"\r  [3/7] Classification complete: {entries.size} declarations    "
    (← IO.getStdout).flush
    return { entries, notes }

  else if proofDepWorkers > 0 then
    -- Parallel mode: extract proof deps in parallel
    classifyAllDeclarationsParallel env modulePrefix proofDepWorkers

  else
    -- Sequential mode with proof deps (original behavior)
    let mut entries : NameMap APIMeta := {}
    let mut notes : Array String := #[]

    let relevantConsts := env.constants.fold (init := #[]) fun acc name cinfo =>
      match env.getModuleIdxFor? name with
      | some modIdx =>
        let modName := env.header.moduleNames[modIdx.toNat]!
        if modulePrefix.isPrefixOf modName then acc.push (name, cinfo) else acc
      | none => acc

    let total := relevantConsts.size
    IO.println s!"  [3/7] Classifying {total} declarations (sequential with proof deps)..."
    (← IO.getStdout).flush

    let mut processed := 0
    let progressInterval := max 1 (total / 20)

    for (name, cinfo) in relevantConsts do
      processed := processed + 1
      if processed % progressInterval == 0 then
        let pct := (processed * 100) / total
        IO.print s!"\r  [3/6] Classifying: {processed}/{total} ({pct}%)    "
        (← IO.getStdout).flush

      if let some apiMeta ← classifyConstant env name cinfo internalPrefixes then
        entries := entries.insert name apiMeta

    IO.println s!"\r  [3/6] Classification complete: {entries.size} declarations    "
    (← IO.getStdout).flush
    return { entries, notes }

/-- Compute the "provedBy" relationship: for each definition, which theorems prove things about it -/
def computeProvedByMap (entries : NameMap APIMeta) : NameMap (Array Name) := Id.run do
  let mut provedBy : NameMap (Array Name) := {}

  for (thmName, apiMeta) in entries do
    if apiMeta.isTheorem then
      -- This theorem proves things about the names in its `proves` field
      for defName in apiMeta.proves do
        let existing := provedBy.find? defName |>.getD #[]
        provedBy := provedBy.insert defName (existing.push thmName)

  return provedBy

/-- Update coverage status based on provedBy relationships -/
def updateCoverageStatus (entries : NameMap APIMeta) (provedBy : NameMap (Array Name)) : NameMap APIMeta := Id.run do
  let mut result := entries

  for (name, apiMeta) in entries do
    if !apiMeta.isTheorem then
      -- Check if any theorems prove things about this definition
      let proofs := provedBy.find? name |>.getD #[]
      if !proofs.isEmpty then
        let newMeta := { apiMeta with coverage := .complete }
        result := result.insert name newMeta

  return result

end DocVerificationBridge
