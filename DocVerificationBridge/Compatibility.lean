-- DocVerificationBridge/Compatibility.lean
-- Cross-version compatibility helpers for Lean 4.24.0 - 4.27.0+

/-!
# Cross-version Compatibility

This module provides compatibility helpers that work across the supported
Lean version range (4.24.0 - 4.27.0+).

## String.trim vs String.trimAscii

In Lean 4.27.0, `String.trim` was deprecated in favor of `String.trimAscii`,
and the return type changed from `String` to `String.Slice`.

We implement our own trim that works consistently across all versions.
-/

/-- Check if a character is ASCII whitespace -/
@[inline] private def isAsciiWhitespace (c : Char) : Bool :=
  c == ' ' || c == '\t' || c == '\n' || c == '\r'

/-- Trim ASCII whitespace from both ends of a string.
    Works across Lean 4.24.0 - 4.27.0+ with consistent String return type. -/
def String.trimCompat (s : String) : String :=
  -- Work with List Char to avoid Slice issues
  let chars := s.toList
  let trimLeft := chars.dropWhile isAsciiWhitespace
  let trimBoth := trimLeft.reverse.dropWhile isAsciiWhitespace |>.reverse
  String.ofList trimBoth
