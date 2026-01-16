-- DocVerificationBridge/VerificationDecorator.lean
-- Provides decoration badges for doc-gen4 declaration pages linking to verification coverage

import Lean
import DocGen4.Output.Base
import DocGen4.Output.ToHtmlFormat
import DocVerificationBridge.Types
import DocVerificationBridge.Classify

open Lean
open scoped DocGen4.Jsx
open DocGen4 (Html)
open DocGen4.Output (DeclarationDecoratorFn)

namespace DocVerificationBridge

/-- Badge style for different verification categories -/
inductive VerificationBadge
  | bridgingSound      -- ⬇️ Soundness theorem
  | bridgingComplete   -- ⬆️ Completeness theorem
  | bridgingIff        -- ⬍ Equivalence theorem
  | mathematical       -- 📐 Mathematical property
  | computational      -- ⚙️ Computational property
  | mathAbstraction    -- 🔷 Mathematical abstraction type
  | compDatatype       -- 🔶 Computational datatype
  | mathDefinition     -- 🔹 Mathematical definition
  | compOperation      -- 🔸 Computational operation
  | unclassified       -- No badge

/-- Get badge info for a verification entry -/
def getBadgeInfo (apiMeta : APIMeta) : VerificationBadge × String × String :=
  match apiMeta.kind with
  | .apiTheorem data =>
    -- Check bridging direction first (most specific)
    match data.bridgingDirection with
    | some .sound => (.bridgingSound, "⬇️", "Soundness: computation implies specification")
    | some .complete => (.bridgingComplete, "⬆️", "Completeness: specification implies computation")
    | some .iff => (.bridgingIff, "⇕", "Equivalence: specification ↔ computation")
    | none =>
      -- Fall back to theorem kind
      match data.kind with
      | some .mathematicalProperty => (.mathematical, "📐", "Mathematical property (specification layer)")
      | some .computationalProperty => (.computational, "⚙️", "Computational property (implementation layer)")
      | some .bridgingProperty => (.bridgingIff, "🔗", "Bridging property")
      | some .soundnessProperty => (.bridgingSound, "⬇️", "Soundness property")
      | some .completenessProperty => (.bridgingComplete, "⬆️", "Completeness property")
      | none => (.unclassified, "", "")
  | .apiType cat =>
    match cat with
    | .mathematicalAbstraction => (.mathAbstraction, "🔷", "Mathematical abstraction type")
    | .computationalDatatype => (.compDatatype, "🔶", "Computational datatype")
  | .apiDef info =>
    match info.category with
    | .mathematicalDefinition => (.mathDefinition, "🔹", "Mathematical definition (Prop-valued)")
    | .computationalOperation => (.compOperation, "🔸", "Computational operation (Bool/decidable)")

/-- CSS class for badge styling -/
def badgeCssClass : VerificationBadge → String
  | .bridgingSound => "verification-badge bridging sound"
  | .bridgingComplete => "verification-badge bridging complete"
  | .bridgingIff => "verification-badge bridging iff"
  | .mathematical => "verification-badge mathematical"
  | .computational => "verification-badge computational"
  | .mathAbstraction => "verification-badge math-abstraction"
  | .compDatatype => "verification-badge comp-datatype"
  | .mathDefinition => "verification-badge math-definition"
  | .compOperation => "verification-badge comp-operation"
  | .unclassified => ""

/-- Generate URL from module name to verification page.
    The module name uses underscores to match the StaticHtml file naming convention:
    e.g., Mathlib.Algebra.AddTorsor.Basic → modules/Mathlib_Algebra_AddTorsor_Basic.html -/
def moduleToVerificationUrl (moduleName : Name) (declName : Name) (baseUrl : String) : String :=
  -- Convert module name to safe filename (using underscores, matching Report.filePathToSafeFilename)
  let safeModuleName := moduleName.toString.replace "." "_"
  s!"{baseUrl}/{safeModuleName}.html#{declName}"

/-- Create a declaration decorator that adds verification badges and links.

    This decorator is passed to doc-gen4's `htmlOutputResults` to inject
    verification badges into each declaration's rendering, enabling
    bidirectional navigation between API docs and verification coverage.

    Parameters:
    - `verificationIndex`: Map from declaration names to their verification metadata
    - `verificationBaseUrl`: Base URL for verification pages (relative to API pages)
      Default "../modules" works when API is at `site/api/` and verification at `site/modules/`
-/
def makeVerificationDecorator
    (verificationIndex : NameMap APIMeta)
    (verificationBaseUrl : String := "../modules")
    : DeclarationDecoratorFn := fun moduleName declName _kind =>
  match verificationIndex.find? declName with
  | none => #[]  -- No verification info for this declaration
  | some apiMeta =>
    let (badge, symbol, tooltip) := getBadgeInfo apiMeta
    if symbol.isEmpty then #[]
    else
      let url := moduleToVerificationUrl moduleName declName verificationBaseUrl
      let cssClass := badgeCssClass badge
      #[
        Html.element "div" false #[("class", "verification_link")] #[
          Html.element "a" false
            #[("href", url), ("class", cssClass), ("title", tooltip)]
            #[Html.text s!"{symbol} coverage"]
        ]
      ]

/-- CSS styles for verification badges (to be injected into doc-gen4 output) -/
def verificationBadgesCss : String := "
/* Verification badge styles for doc-gen4 integration */
.verification_link {
  display: inline-block;
  margin-left: 0.5em;
  font-size: 0.85em;
}

.verification-badge {
  padding: 2px 6px;
  border-radius: 4px;
  text-decoration: none;
  font-weight: 500;
}

.verification-badge.bridging {
  background-color: #e8f5e9;
  color: #2e7d32;
  border: 1px solid #a5d6a7;
}

.verification-badge.bridging:hover {
  background-color: #c8e6c9;
}

.verification-badge.mathematical {
  background-color: #e3f2fd;
  color: #1565c0;
  border: 1px solid #90caf9;
}

.verification-badge.mathematical:hover {
  background-color: #bbdefb;
}

.verification-badge.computational {
  background-color: #fff3e0;
  color: #e65100;
  border: 1px solid #ffcc80;
}

.verification-badge.computational:hover {
  background-color: #ffe0b2;
}

.verification-badge.math-abstraction,
.verification-badge.math-definition {
  background-color: #ede7f6;
  color: #4527a0;
  border: 1px solid #b39ddb;
}

.verification-badge.comp-datatype,
.verification-badge.comp-operation {
  background-color: #fce4ec;
  color: #c2185b;
  border: 1px solid #f48fb1;
}

/* Dark mode support */
@media (prefers-color-scheme: dark) {
  .verification-badge.bridging {
    background-color: #1b5e20;
    color: #a5d6a7;
    border-color: #388e3c;
  }

  .verification-badge.mathematical {
    background-color: #0d47a1;
    color: #90caf9;
    border-color: #1976d2;
  }

  .verification-badge.computational {
    background-color: #e65100;
    color: #ffe0b2;
    border-color: #f57c00;
  }
}
"

end DocVerificationBridge
