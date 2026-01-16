-- DocVerificationBridge/VerificationDecoratorStandard.lean
-- Standard version: Provides stub types when using official doc-gen4 (without decorator support)
-- This file is copied to docvb/ for projects using official doc-gen4 releases

import Lean
import DocGen4.Output.ToHtmlFormat
import DocVerificationBridge.Types
import DocVerificationBridge.Classify

open Lean
open scoped DocGen4.Jsx
open DocGen4 (Html)

namespace DocVerificationBridge

/-- Stub type for declaration decorator when using official doc-gen4.
    The actual decorator functionality requires the NicolasRouquette/doc-gen4 fork. -/
abbrev DeclarationDecoratorFn := Name → Name → String → Array Html

/-- Create a no-op decorator (official doc-gen4 doesn't support decoration injection).
    When using this with the standard doc-gen4, the decorator will be ignored.

    Parameters:
    - `verificationIndex`: Map from declaration names to their verification metadata
    - `verificationBaseUrl`: Base URL for verification pages (unused in standard mode)
-/
def makeVerificationDecorator
    (_verificationIndex : NameMap APIMeta)
    (_verificationBaseUrl : String := "../modules")
    : DeclarationDecoratorFn := fun _moduleName _declName _kind => #[]

/-- CSS styles for verification badges (empty in standard mode) -/
def verificationBadgesCss : String := ""

end DocVerificationBridge
