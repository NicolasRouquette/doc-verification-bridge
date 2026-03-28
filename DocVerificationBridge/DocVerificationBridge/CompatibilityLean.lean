-- DocVerificationBridge/CompatibilityLean.lean
-- Cross-version compatibility tests that require `import Lean`

import Lean

/-!
# Lean-Specific Compatibility Tests

This module tests idioms that require `import Lean` and must work across
all supported Lean versions (see `minSupportedVersion`/`maxSupportedVersion`
in ExperimentsCore.lean for the authoritative range).

## Options API Changes

The `Lean.Options` type has changed its constructor visibility:
- Lean < 4.28.0: Can use anonymous constructor `⟨[...]⟩`
- Lean >= 4.28.0: Constructor is private, must use `Options.empty` + `setBool`/etc.

We use the newer pattern which works across all versions.
-/

open Lean

namespace DocVerificationBridge.CompatibilityLean

/-- Test that Options.empty and setBool work (required for MetaM config).
    This pattern works across all supported Lean versions. -/
def testOptionsConstruction : Options :=
  Options.empty
    |>.setBool `pp.tagAppFns true
    |>.setBool `pp.funBinderTypes true
    |>.setBool `debug.skipKernelTC true
    |>.setBool `Elab.async false

-- Just verify we can call getBool (compile-time check only)
#check testOptionsConstruction.getBool `pp.tagAppFns
#check testOptionsConstruction.getBool `Elab.async

/-- Test Core.Context construction with Options -/
def testCoreContext : Core.Context := {
  maxHeartbeats := 100000000,
  options := Options.empty
    |>.setBool `pp.tagAppFns true
    |>.setBool `pp.funBinderTypes true,
  fileName := default,
  fileMap := default,
}

-- Verify context options field is accessible
#check testCoreContext.options

end DocVerificationBridge.CompatibilityLean
