-- DocVerificationBridge/Attributes.lean
-- Attribute syntax for manual annotation (optional, for precise control)

import Lean
import DocVerificationBridge.Types

/-!
# Optional Annotation Attributes

These attributes provide manual control over classification when automatic
inference is insufficient or when precise annotations are desired.

## Usage

```lean
@[api_type { category := .mathematicalAbstraction }]
class MapLike (M : Type*) (K V : Type*) where ...

@[api_def]  -- category auto-detected from return type
def lookup (m : M) (k : K) : Option V := ...

@[api_theorem {
  theoremKind := .bridgingProperty,
  proves := #[`lookupProp],
  validates := #[`lookupBool]
}]
theorem lookup_sound : lookupBool m k = true → lookupProp m k := ...
```
-/

namespace DocVerificationBridge

open Lean
open Lean.Elab
open Lean.Elab.Command

/-!
## Attribute Syntax
-/

/-- Syntax for optional metadata block -/
syntax apiMetaBlock := "{" (ident ":=" term),* "}"

/-- Mark as a tracked API TYPE (for structure/inductive/class) with required category -/
syntax (name := apiTypeAttr) "api_type" (apiMetaBlock)? : attr

/-- Mark as a tracked API definition (for def) with optional metadata.
    Category is auto-detected from return type -/
syntax (name := apiDefAttr) "api_def" (apiMetaBlock)? : attr

/-- Mark as an API theorem with optional metadata -/
syntax (name := apiTheoremAttr) "api_theorem" (apiMetaBlock)? : attr

/-- Mark as an API lemma with optional metadata (synonym for api_theorem) -/
syntax (name := apiLemmaAttr) "api_lemma" (apiMetaBlock)? : attr

/-!
## Parsing Helpers
-/

/-- Parse a TypeCategory from syntax -/
def parseTypeCategory (stx : Syntax) : Option TypeCategory :=
  match stx with
  | `(term| .mathematicalAbstraction) => some .mathematicalAbstraction
  | `(term| .computationalDatatype) => some .computationalDatatype
  | `(term| TypeCategory.mathematicalAbstraction) => some .mathematicalAbstraction
  | `(term| TypeCategory.computationalDatatype) => some .computationalDatatype
  | _ => none

/-- Parse a CoverageStatus from syntax -/
def parseCoverage (stx : Syntax) : Option CoverageStatus :=
  match stx with
  | `(term| .complete) => some .complete
  | `(term| .axiomDependent) => some .axiomDependent
  | `(term| .partialCoverage) => some .partialCoverage
  | `(term| .unverified) => some .unverified
  | `(term| .specOnly) => some .specOnly
  | `(term| .notApplicable) => some .notApplicable
  | _ => none

/-- Parse a TheoremKind from syntax -/
def parseTheoremKind (stx : Syntax) : Option TheoremKind :=
  match stx with
  | `(term| .computationalProperty) => some .computationalProperty
  | `(term| .mathematicalProperty) => some .mathematicalProperty
  | `(term| .bridgingProperty) => some .bridgingProperty
  | `(term| .soundnessProperty) => some .soundnessProperty
  | `(term| .completenessProperty) => some .completenessProperty
  | _ => none

/-- Parse a BridgingDirection from syntax -/
def parseBridgingDirection (stx : Syntax) : Option BridgingDirection :=
  match stx with
  | `(term| .sound) => some .sound
  | `(term| .complete) => some .complete
  | `(term| .iff) => some .iff
  | _ => none

end DocVerificationBridge
