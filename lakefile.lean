import Lake
open Lake DSL

package «doc-verification-bridge» where
  -- Settings for the package

lean_lib DocVerificationBridge where
  -- Library configuration

-- Unified CLI combining doc-gen4 + verification (main executable)
@[default_target]
lean_exe «unified-doc» where
  root := `UnifiedMain
  supportInterpreter := true

-- Experiments runner for batch analysis of multiple projects
lean_exe «experiments» where
  root := `ExperimentsMain
  supportInterpreter := true

-- Dependencies
require «doc-gen4» from git
  "https://github.com/NicolasRouquette/doc-gen4" @ "fix/lean-4.29.0-rc2"

require Cli from git
  "https://github.com/leanprover/lean4-cli" @ "main"
