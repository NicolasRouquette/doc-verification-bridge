import Lake
open Lake DSL

package «doc-verification-bridge» where
  -- Settings for the package

lean_lib DocVerificationBridge where
  -- Library configuration

-- Original standalone CLI (verification only, MkDocs output)
@[default_target]
lean_exe «doc-verification-bridge» where
  root := `Main
  supportInterpreter := true

-- Unified CLI combining doc-gen4 + verification
lean_exe «unified-doc» where
  root := `UnifiedMain
  supportInterpreter := true

-- Dependencies
require «doc-gen4» from git
  "https://github.com/leanprover/doc-gen4" @ "main"

require Cli from git
  "https://github.com/leanprover/lean4-cli" @ "main"
