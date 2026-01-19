-- DocVerificationBridge/Compatibility.lean
-- Cross-version compatibility helpers for Lean 4.24.0 - 4.27.0+

/-!
# Cross-version Compatibility

This module provides compatibility helpers that work across the supported
Lean version range (4.24.0 - 4.27.0+).

## String Slice Changes in Lean 4.27.0

In Lean 4.27.0, many String operations that returned `String` now return `String.Slice`:
- `String.trim` → `String.trimAscii` (returns Slice)
- `String.drop` (returns Slice)
- `String.take` (returns Slice)
- `String.splitOn` (returns List of Slices)
- `String.startsWith` / `String.endsWith` (may involve Slices)

We implement our own versions that work consistently across all versions
by using character-level operations.
-/

/-- Check if a character is ASCII whitespace -/
@[inline] private def isAsciiWhitespace (c : Char) : Bool :=
  c == ' ' || c == '\t' || c == '\n' || c == '\r'

/-- Convert a list of characters to a String.
    Compatibility helper for Lean < 4.26.0 which doesn't have String.ofList. -/
@[inline] private def listToString (chars : List Char) : String :=
  chars.foldl (init := "") fun acc c => acc.push c

/-- Trim ASCII whitespace from both ends of a string.
    Works across Lean 4.24.0 - 4.27.0+ with consistent String return type. -/
def String.trimCompat (s : String) : String :=
  -- Work with List Char to avoid Slice issues
  let chars := s.toList
  let trimLeft := chars.dropWhile isAsciiWhitespace
  let trimBoth := trimLeft.reverse.dropWhile isAsciiWhitespace |>.reverse
  listToString trimBoth

/-- Drop n characters from the beginning of a string.
    Works across Lean 4.24.0 - 4.27.0+ with consistent String return type. -/
def String.dropCompat (s : String) (n : Nat) : String :=
  listToString (s.toList.drop n)

/-- Take n characters from the beginning of a string.
    Works across Lean 4.24.0 - 4.27.0+ with consistent String return type. -/
def String.takeCompat (s : String) (n : Nat) : String :=
  listToString (s.toList.take n)

/-- Split a string on a separator, returning a list of Strings.
    Works across Lean 4.24.0 - 4.27.0+ with consistent String return type. -/
partial def String.splitOnCompat (s : String) (sep : String) : List String :=
  if sep.isEmpty then [s]
  else
    let sepChars := sep.toList
    go s.toList sepChars [] []
where
  /-- Check if list starts with prefix -/
  startsWith (xs : List Char) (start_prefix : List Char) : Bool :=
    match start_prefix, xs with
    | [], _ => true
    | _ :: _, [] => false
    | p :: ps, x :: rest => p == x && startsWith rest ps

  /-- Main splitting loop -/
  go (remaining : List Char) (sepChars : List Char) (current : List Char) (acc : List String) : List String :=
    match remaining with
    | [] => acc ++ [listToString current.reverse]
    | c :: rest =>
      -- Check if remaining starts with separator
      if startsWith remaining sepChars then
        -- Found separator: add current segment, skip separator chars
        let newAcc := acc ++ [listToString current.reverse]
        go (remaining.drop sepChars.length) sepChars [] newAcc
      else
        -- No match: add char to current segment
        go rest sepChars (c :: current) acc

/-- Drop characters from the end of a string.
    Works across Lean 4.24.0 - 4.27.0+ with consistent String return type. -/
def String.dropRightCompat (s : String) (n : Nat) : String :=
  let chars := s.toList
  listToString (chars.take (chars.length - n))

/-- Take characters from the end of a string.
    Works across Lean 4.24.0 - 4.27.0+ with consistent String return type. -/
def String.takeRightCompat (s : String) (n : Nat) : String :=
  let chars := s.toList
  listToString (chars.drop (chars.length - n))

/-- Trim whitespace from the left side of a string.
    Compatible across Lean versions. -/
def String.trimLeftCompat (s : String) : String :=
  let chars := s.toList.dropWhile fun c => c.isWhitespace
  listToString chars

/-- Check if string starts with prefix (returns Bool, not dependent on Slice).
    This is a compatibility helper that explicitly returns Bool. -/
@[inline] def String.startsWithCompat (s : String) (start_prefix : String) : Bool :=
  s.takeCompat start_prefix.length == start_prefix

/-- Check if string ends with suffix (returns Bool, not dependent on Slice).
    This is a compatibility helper that explicitly returns Bool. -/
@[inline] def String.endsWithCompat (s : String) (end_suffix : String) : Bool :=
  s.takeRightCompat end_suffix.length == end_suffix

/-- Check if string contains a substring.
    This is a compatibility helper that explicitly returns Bool. -/
@[inline] def String.containsCompat (s : String) (sub : String) : Bool :=
  (s.splitOnCompat sub).length > 1

/-- Find the first occurrence of a substring in a string.
    Returns the character index (not byte position) if found, or none.
    This is a compatibility helper for cross-version support. -/
def String.findOccurrenceCompat (s : String) (sub : String) : Option Nat :=
  if sub.isEmpty then some 0
  else
    let chars := s.toList
    let subChars := sub.toList
    let rec go (remaining : List Char) (idx : Nat) : Option Nat :=
      match remaining with
      | [] => none
      | _ :: rest =>
        if remaining.take subChars.length == subChars then some idx
        else go rest (idx + 1)
    go chars 0
