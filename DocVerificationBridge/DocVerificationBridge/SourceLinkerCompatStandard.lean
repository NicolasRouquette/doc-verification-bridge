-- DocVerificationBridge/SourceLinkerCompatStandard.lean
-- Standard version: Uses official doc-gen4 3-argument API
-- This file is copied to docvb/ for projects using official doc-gen4 releases

import Lean
import DocGen4
import DocGen4.Output

open Lean DocGen4 DocGen4.Output DocGen4.Process

namespace DocVerificationBridge

/-- Compatibility shim for doc-gen4 HTML output.
    This version uses the standard 3-argument API available in all doc-gen4 releases.
    Custom linker and decorator are ignored - they require the NicolasRouquette fork. -/
def htmlOutputResultsCompat
    (baseConfig : SiteBaseContext)
    (result : AnalyzerResult)
    (sourceUrl? : Option String)
    (_customLinker? : Option (Name → Option DeclarationRange → String) := none)
    (_declarationDecorator? : Option (Name → Name → String → Array DocGen4.Html) := none)
    : IO (Array System.FilePath) := do
  -- Standard API ignores custom linker and decorator
  DocGen4.htmlOutputResults baseConfig result sourceUrl?

end DocVerificationBridge
