import Lake
open Lake DSL

package «doc-verification-bridge» where
  -- Settings for the package

lean_lib DocVerificationBridge where
  -- Library configuration

@[default_target]
lean_exe «doc-verification-bridge» where
  root := `Main
  supportInterpreter := true

-- Dependencies
require «doc-gen4» from git
  "https://github.com/leanprover/doc-gen4" @ "main"

require Cli from git
  "https://github.com/leanprover/lean4-cli" @ "main"
