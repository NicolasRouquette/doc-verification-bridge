-- DocVerificationBridge/SourceLinkerCompat.lean
-- Standard version: Uses official doc-gen4 3-argument API
-- This file is copied to docvb/ for projects using official doc-gen4 releases

import Lean
import DocGen4
import DocGen4.Output

open Lean DocGen4 DocGen4.Output DocGen4.Process

namespace DocVerificationBridge

/-- Compatibility shim for doc-gen4 HTML output.
    This version uses the standard 3-argument API available in all doc-gen4 releases. -/
def htmlOutputResultsCompat
    (baseConfig : SiteBaseContext)
    (result : AnalyzerResult)
    (sourceUrl? : Option String)
    (_customLinker? : Option (Name → Option DeclarationRange → String) := none)
    : IO (Array System.FilePath) := do
  -- Standard API ignores custom linker - doc-gen4 uses its built-in source linker
  DocGen4.htmlOutputResults baseConfig result sourceUrl?

end DocVerificationBridge
