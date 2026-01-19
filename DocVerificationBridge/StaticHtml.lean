/-
Copyright (c) 2026 California Institute of Technology. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Nicolas Rouquette
-/
import Lean
import DocGen4.Output.ToHtmlFormat
import DocGen4.Output.Base
import DocVerificationBridge.Classify
import DocVerificationBridge.Report
import DocVerificationBridge.Compatibility

/-!
# Static HTML Generation

This module provides a static HTML generation pipeline.
It generates styled HTML output with parallel HTML generation directly from Lean.

## Architecture

1. Generate HTML files directly (parallel Lean tasks)
2. Copy static assets (CSS/JS)
3. Generate search index
4. Done in minutes

## Reused from doc-gen4

- `Html` type and JSX DSL
- CSS styling patterns
- Search index format
- Navigation structure
-/

namespace DocVerificationBridge.StaticHtml

open Lean
open scoped DocGen4.Jsx
open DocGen4 (Html Hierarchy)
open DocGen4.Output (BaseHtmlM SiteBaseContext BibItem)

/-! ## Configuration -/

/-- Configuration for static HTML generation -/
structure StaticHtmlConfig where
  /-- Output directory for generated site -/
  outputDir : System.FilePath
  /-- Project name for titles -/
  projectName : String
  /-- Repository URL for source links -/
  repoUrl : String
  /-- Git branch -/
  branch : String
  /-- Base URL for API docs (relative path to doc-gen4 output) -/
  apiBaseUrl : String := "api"
  /-- Number of parallel workers for HTML generation -/
  workers : Nat := 20
  /-- Optional directory containing data files (classification-cache.json, etc.)
      If set, download links will be shown on the index page -/
  dataFilesDir : Option System.FilePath := none
  /-- Optional project description (from config.toml) -/
  projectDescription : Option String := none
  /-- Optional list of top-level modules being analyzed -/
  projectModules : Array String := #[]
  /-- Optional config settings to display (key-value pairs) -/
  projectSettings : Array (String × String) := #[]
  deriving Repr, Inhabited

/-! ## HTML Helpers -/

/-- Escape HTML special characters (delegates to doc-gen4's Html.escape) -/
def escapeHtml (s : String) : String := DocGen4.Html.escape s

/-- Process bold markdown: **text** → <strong>text</strong> -/
def processBoldMarkdown (input : String) : String :=
  let parts := input.splitOnCompat "**"
  if parts.length < 3 then input
  else
    let rec goBold (ps : List String) (inBold : Bool) (acc : String) : String :=
      match ps with
      | [] => acc
      | p :: rest =>
        if inBold then goBold rest false (acc ++ "<strong>" ++ p ++ "</strong>")
        else goBold rest true (acc ++ p)
    goBold parts false ""

/-- Process inline code markdown: `text` → <code>text</code> -/
def processCodeMarkdown (input : String) : String :=
  let parts := input.splitOnCompat "`"
  if parts.length < 3 then input
  else
    let rec goCode (ps : List String) (inCode : Bool) (acc : String) : String :=
      match ps with
      | [] => acc
      | p :: rest =>
        if inCode then goCode rest false (acc ++ "<code>" ++ p ++ "</code>")
        else goCode rest true (acc ++ p)
    goCode parts false ""

/-- Process markdown links: [text](url "title"){#anchor} → <a href="url" title="title" id="anchor">text</a> -/
partial def processLinksMarkdown (input : String) : String :=
  let rec goLinks (remaining : String) (acc : String) : String :=
    match remaining.splitOnCompat "[" with
    | [noLink] => acc ++ noLink
    | before :: afterOpen =>
      let afterOpenStr := String.intercalate "[" afterOpen
      match afterOpenStr.splitOnCompat "](" with
      | [noClose] => acc ++ before ++ "[" ++ noClose
      | textPart :: afterText =>
        let afterTextStr := String.intercalate "](" afterText
        match afterTextStr.splitOnCompat ")" with
        | [noClose] => acc ++ before ++ "[" ++ textPart ++ "](" ++ noClose
        | urlPart :: restParts =>
          let rest := String.intercalate ")" restParts
          -- Parse URL and optional title: url "title" or just url
          let (url, titleAttr) :=
            -- Check for title in format: url "title" or url 'title'
            if let some idx := urlPart.findOccurrenceCompat " \"" then
              let urlOnly := urlPart.takeCompat idx
              let titlePart := urlPart.dropCompat (idx + 2)
              -- Remove trailing quote
              let title := if titlePart.endsWith "\"" then
                titlePart.dropRightCompat 1
              else titlePart
              (urlOnly, s!" title=\"{title}\"")
            else
              (urlPart, "")
          -- Check if there's an anchor ID AFTER the link: {#anchor-id}
          let (anchorId, restAfterAnchor) := if rest.trimLeftCompat.startsWithCompat "{#" then
            -- Find the closing }
            match rest.splitOnCompat "}" with
            | anchorPart :: afterBrace =>
              -- Extract just the ID part (after {#)
              match anchorPart.splitOnCompat "{#" with
              | _ :: idPart :: _ =>
                (some idPart, String.intercalate "}" afterBrace)
              | _ => (none, rest)
            | [] => (none, rest)
          else (none, rest)
          let anchor := match anchorId with
            | some id => s!" id=\"{id}\""
            | none => ""
          let link := s!"<a href=\"{url}\"{titleAttr}{anchor}>{textPart}</a>"
          goLinks restAfterAnchor (acc ++ before ++ link)
        | [] => acc ++ before
      | [] => acc ++ before
    | [] => acc
  goLinks input ""

/-- Convert inline markdown to HTML (bold, links, code, icons) -/
def processInlineMarkdown (s : String) : String :=
  -- Process Material Design icons: :material-*: → 📖 icon
  let s := if s.containsCompat ":material-book-open-page-variant:" then
    s.replace ":material-book-open-page-variant:" "📖"
  else s
  -- Process bold
  let s := processBoldMarkdown s
  -- Process inline code
  let s := processCodeMarkdown s
  -- Process links
  let s := processLinksMarkdown s
  s

/-- State for markdown-to-HTML conversion -/
structure MdToHtmlState where
  result : Array String := #[]
  inTable : Bool := false
  isHeaderRow : Bool := true
  inList : Bool := false

/-- Convert markdown-style code blocks to HTML -/
def markdownToHtml (md : String) : String :=
  -- Simple markdown conversion for verification reports
  -- Handles: headers, code blocks, lists, bold, links, tables
  let lines := md.splitOn "\n"

  let processLine (state : MdToHtmlState) (line : String) : MdToHtmlState :=
    -- Handle table separator lines (skip them but keep table state)
    if line.trimCompat.startsWithCompat "|" && line.containsCompat "---" then
      -- This is a table separator line, skip it
      state
    -- Handle table rows
    else if line.startsWithCompat "| " || (line.startsWithCompat "|" && line.length > 1) then
      let state := if !state.inTable then
        -- Start new table
        let state := if state.inList then { state with result := state.result.push "</ul>", inList := false } else state
        { state with result := state.result.push "<table class=\"verification-table\">", inTable := true, isHeaderRow := true }
      else state

      let trimmedLine := if line.startsWithCompat "| " then line.dropCompat 2 else line.dropCompat 1
      let trimmedLine := if trimmedLine.endsWithCompat " |" then trimmedLine.dropRightCompat 2
                         else if trimmedLine.endsWithCompat "|" then trimmedLine.dropRightCompat 1
                         else trimmedLine
      let cells := trimmedLine.splitOnCompat " | "
      let tag := if state.isHeaderRow then "th" else "td"
      -- Process inline markdown within each cell
      let cellsHtml := cells.map (fun c => s!"<{tag}>{processInlineMarkdown c}</{tag}>") |>.foldl (· ++ ·) ""
      { state with result := state.result.push s!"<tr>{cellsHtml}</tr>", isHeaderRow := false }
    else
      -- End table if we were in one
      let state := if state.inTable then
        { state with result := state.result.push "</table>", inTable := false, isHeaderRow := true }
      else state

      -- Handle headers
      if line.startsWithCompat "### " then
        let state := if state.inList then { state with result := state.result.push "</ul>", inList := false } else state
        { state with result := state.result.push s!"<h3>{escapeHtml (line.dropCompat 4)}</h3>" }
      else if line.startsWithCompat "## " then
        let state := if state.inList then { state with result := state.result.push "</ul>", inList := false } else state
        { state with result := state.result.push s!"<h2>{escapeHtml (line.dropCompat 3)}</h2>" }
      else if line.startsWithCompat "# " then
        let state := if state.inList then { state with result := state.result.push "</ul>", inList := false } else state
        { state with result := state.result.push s!"<h1>{escapeHtml (line.dropCompat 2)}</h1>" }
      else if line.startsWithCompat "- " then
        let state := if !state.inList then { state with result := state.result.push "<ul>", inList := true } else state
        { state with result := state.result.push s!"<li>{processInlineMarkdown (line.dropCompat 2)}</li>" }
      else if line.startsWithCompat "```" then
        let state := if state.inList then { state with result := state.result.push "</ul>", inList := false } else state
        if line.length > 3 then
          { state with result := state.result.push "<pre><code>" }
        else
          { state with result := state.result.push "</code></pre>" }
      else if line.trimCompat.isEmpty then
        -- Empty line - close list if needed, otherwise skip
        if state.inList then { state with result := state.result.push "</ul>", inList := false }
        else state
      else
        let state := if state.inList then { state with result := state.result.push "</ul>", inList := false } else state
        { state with result := state.result.push s!"<p>{processInlineMarkdown line}</p>" }

  let finalState := lines.foldl processLine {}

  -- Close any open elements
  let finalState := if finalState.inTable then
    { finalState with result := finalState.result.push "</table>" }
  else finalState
  let finalState := if finalState.inList then
    { finalState with result := finalState.result.push "</ul>" }
  else finalState

  String.intercalate "\n" finalState.result.toList

/-! ## CSS and JavaScript -/

/-- Main CSS for verification pages -/
def verificationCss : String := "
/* Verification Report Styles */
:root {
  --bg-color: #ffffff;
  --text-color: #333333;
  --link-color: #0066cc;
  --border-color: #e0e0e0;
  --header-bg: #f5f5f5;
  --success-color: #28a745;
  --warning-color: #ffc107;
  --danger-color: #dc3545;
  --info-color: #17a2b8;
  --code-bg: #f4f4f4;
}

@media (prefers-color-scheme: dark) {
  :root {
    --bg-color: #1a1a2e;
    --text-color: #e0e0e0;
    --link-color: #6db3f2;
    --border-color: #404040;
    --header-bg: #252540;
    --code-bg: #2d2d44;
  }
}

* { box-sizing: border-box; }

body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, sans-serif;
  line-height: 1.6;
  color: var(--text-color);
  background-color: var(--bg-color);
  margin: 0;
  padding: 0;
}

.container {
  max-width: 1200px;
  margin: 0 auto;
  padding: 20px;
}

/* Header */
header {
  background: var(--header-bg);
  border-bottom: 1px solid var(--border-color);
  padding: 15px 20px;
  position: sticky;
  top: 0;
  z-index: 100;
}

header h1 {
  margin: 0;
  font-size: 1.5rem;
}

header nav {
  margin-top: 10px;
}

header nav a {
  margin-right: 20px;
  text-decoration: none;
  color: var(--link-color);
}

/* Navigation sidebar */
.sidebar {
  position: fixed;
  left: 0;
  top: 60px;
  width: 280px;
  height: calc(100vh - 60px);
  overflow-y: auto;
  background: var(--header-bg);
  border-right: 1px solid var(--border-color);
  padding: 15px;
}

.sidebar ul {
  list-style: none;
  padding-left: 15px;
  margin: 5px 0;
}

.sidebar > ul {
  padding-left: 0;
}

.sidebar a {
  color: var(--text-color);
  text-decoration: none;
  display: block;
  padding: 3px 0;
}

.sidebar a:hover {
  color: var(--link-color);
}

/* Main content */
main {
  margin-left: 300px;
  padding: 20px;
}

@media (max-width: 768px) {
  .sidebar { display: none; }
  main { margin-left: 0; }
}

/* Tables */
table {
  width: 100%;
  border-collapse: collapse;
  margin: 20px 0;
}

th, td {
  padding: 10px;
  border: 1px solid var(--border-color);
  text-align: left;
}

th {
  background: var(--header-bg);
  font-weight: 600;
}

/* Verification tables */
.verification-table {
  width: 100%;
  border-collapse: collapse;
  margin: 15px 0;
  font-size: 0.9rem;
}

.verification-table th,
.verification-table td {
  padding: 8px 12px;
  border: 1px solid var(--border-color);
  text-align: left;
  vertical-align: top;
}

.verification-table th {
  background: var(--header-bg);
  font-weight: 600;
  white-space: nowrap;
}

.verification-table tr:hover {
  background: rgba(0, 102, 204, 0.05);
}

.verification-table a {
  color: var(--link-color);
  text-decoration: none;
}

.verification-table a:hover {
  text-decoration: underline;
}

.verification-table code {
  background: var(--code-bg);
  padding: 1px 4px;
  border-radius: 3px;
  font-size: 0.85em;
}

/* Status badges */
.badge {
  display: inline-block;
  padding: 2px 8px;
  border-radius: 4px;
  font-size: 0.85rem;
  font-weight: 500;
}

.badge-proved { background: var(--success-color); color: white; }
.badge-axiom { background: var(--info-color); color: white; }
.badge-sorry { background: var(--danger-color); color: white; }
.badge-def { background: var(--warning-color); color: black; }

/* Code blocks */
pre, code {
  font-family: 'SF Mono', Monaco, 'Cascadia Code', monospace;
  background: var(--code-bg);
  border-radius: 4px;
}

pre {
  padding: 15px;
  overflow-x: auto;
}

code {
  padding: 2px 6px;
}

/* Declaration cards */
.decl-card {
  border: 1px solid var(--border-color);
  border-radius: 8px;
  margin: 15px 0;
  overflow: hidden;
}

.decl-header {
  background: var(--header-bg);
  padding: 10px 15px;
  border-bottom: 1px solid var(--border-color);
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.decl-body {
  padding: 15px;
}

.decl-name {
  font-family: monospace;
  font-weight: 600;
}

/* Search */
#search-input {
  width: 100%;
  padding: 10px 15px;
  border: 1px solid var(--border-color);
  border-radius: 4px;
  font-size: 1rem;
  background: var(--bg-color);
  color: var(--text-color);
}

#search-results {
  margin-top: 20px;
}

/* Progress bars */
.progress-bar {
  height: 20px;
  background: var(--border-color);
  border-radius: 4px;
  overflow: hidden;
}

.progress-fill {
  height: 100%;
  transition: width 0.3s ease;
}

.progress-proved { background: var(--success-color); }
.progress-axiom { background: var(--info-color); }
.progress-sorry { background: var(--danger-color); }

/* Links */
a { color: var(--link-color); }
a:hover { text-decoration: underline; }

/* Collapsible sections */
details {
  border: 1px solid var(--border-color);
  border-radius: 4px;
  margin: 10px 0;
}

details summary {
  padding: 10px 15px;
  cursor: pointer;
  background: var(--header-bg);
  font-weight: 500;
}

details[open] summary {
  border-bottom: 1px solid var(--border-color);
}

details > *:not(summary) {
  padding: 15px;
}

/* Ontology and Taxonomy tables */
.ontology-section,
.taxonomy-section {
  margin: 30px 0;
}

.ontology-table {
  width: 100%;
  border-collapse: collapse;
  margin: 15px 0;
}

.ontology-table th,
.ontology-table td {
  padding: 12px 15px;
  border: 1px solid var(--border-color);
  text-align: center;
}

.ontology-table th:first-child,
.ontology-table td:first-child {
  text-align: left;
  font-weight: 600;
}

.ontology-table thead th {
  background: var(--header-bg);
  font-weight: 600;
}

.ontology-table tbody th {
  background: var(--header-bg);
  width: 200px;
}

/* Indent and muted styles */
.indent {
  padding-left: 30px !important;
}

.muted {
  color: #888;
}
"

/-- JavaScript for search and interactivity -/
def verificationJs : String := "
// Search functionality
const searchInput = document.getElementById('search-input');
const searchResults = document.getElementById('search-results');

let declarations = [];

// Load declaration index
async function loadDeclarations() {
  try {
    const response = await fetch('declarations.json');
    declarations = await response.json();
  } catch (e) {
    console.error('Failed to load declarations:', e);
  }
}

// Search function
function search(query) {
  if (!query || query.length < 2) {
    searchResults.innerHTML = '';
    return;
  }

  const q = query.toLowerCase();
  const matches = declarations.filter(d =>
    d.name.toLowerCase().includes(q) ||
    (d.module && d.module.toLowerCase().includes(q))
  ).slice(0, 50);

  if (matches.length === 0) {
    searchResults.innerHTML = '<p>No results found.</p>';
    return;
  }

  const html = matches.map(d => `
    <div class=\"decl-card\">
      <div class=\"decl-header\">
        <span class=\"decl-name\">${escapeHtml(d.name)}</span>
        <span class=\"badge badge-${d.status}\">${d.status}</span>
      </div>
      <div class=\"decl-body\">
        <a href=\"modules/${d.module.replace(/\\./g, '_')}.html#${d.name}\">View in ${d.module}</a>
      </div>
    </div>
  `).join('');

  searchResults.innerHTML = html;
}

function escapeHtml(s) {
  return s.replace(/&/g, '&amp;')
          .replace(/</g, '&lt;')
          .replace(/>/g, '&gt;')
          .replace(/\"/g, '&quot;');
}

// Event listeners
if (searchInput) {
  searchInput.addEventListener('input', (e) => search(e.target.value));
  loadDeclarations();
}

// Collapsible navigation
document.querySelectorAll('.nav-toggle').forEach(toggle => {
  toggle.addEventListener('click', () => {
    toggle.parentElement.classList.toggle('expanded');
  });
});
"

/-! ## HTML Page Templates -/

/-- Generate the base HTML structure for a page.
    `depthToRoot` indicates how many directory levels deep we are from the site root.
    0 = at root (e.g., index.html), 1 = one level deep (e.g., modules/index.html) -/
def baseHtml (title : String) (content : Html) (cfg : StaticHtmlConfig)
    (breadcrumbs : List (String × String) := []) (depthToRoot : Nat := 0) : BaseHtmlM Html := pure <|
  let pathPrefix := String.intercalate "/" (List.replicate depthToRoot "..")
  let slashPrefix := if pathPrefix.isEmpty then "" else pathPrefix ++ "/"
  let breadcrumbItems : Array Html := breadcrumbs.toArray.map fun (name, url) =>
    if url.isEmpty then Html.text name
    else Html.element "span" true #[] #[
      Html.element "a" true #[("href", url)] #[Html.text name],
      Html.text " / "
    ]
  let breadcrumbNav : Array Html :=
    if breadcrumbs.isEmpty then #[]
    else #[Html.element "nav" false #[("class", "breadcrumbs")] breadcrumbItems]

  Html.element "html" false #[("lang", "en")] #[
    Html.element "head" false #[] #[
      Html.element "meta" true #[("charset", "UTF-8")] #[],
      Html.element "meta" true #[("name", "viewport"), ("content", "width=device-width, initial-scale=1.0")] #[],
      Html.element "title" true #[] #[Html.text s!"{title} - {cfg.projectName} Verification"],
      Html.element "link" true #[("rel", "stylesheet"), ("href", s!"{slashPrefix}style.css")] #[],
      Html.element "script" true #[("defer", "true"), ("src", s!"{slashPrefix}verification.js")] #[]
    ],
    Html.element "body" false #[] #[
      Html.element "header" false #[] <| #[
        Html.element "h1" true #[] #[Html.text s!"{cfg.projectName} Verification Report"],
        Html.element "nav" false #[] #[
          Html.element "a" true #[("href", s!"{slashPrefix}index.html")] #[Html.text "Overview"],
          Html.element "a" true #[("href", s!"{slashPrefix}modules/index.html")] #[Html.text "Modules"],
          Html.element "a" true #[("href", s!"{slashPrefix}search.html")] #[Html.text "Search"],
          Html.element "a" true #[("href", s!"{slashPrefix}{cfg.apiBaseUrl}/index.html")] #[Html.text "API Docs"]
        ]
      ] ++ breadcrumbNav,
      Html.element "main" false #[] #[content]
    ]
  ]

/-- Generate a module navigation tree -/
def generateNavTree (modules : List String) : BaseHtmlM Html := pure <| Id.run do
  -- Group modules by module prefix using simple accumulator
  let grouped : List (String × List String) := modules.foldl (init := []) fun acc mod =>
    let parts := mod.splitOn "."
    let modPrefix := if parts.length > 1 then parts.head! else ""
    -- Find existing entry for this prefix or add new one
    let rec updateOrAdd (xs : List (String × List String)) : List (String × List String) :=
      match xs with
      | [] => [(modPrefix, [mod])]
      | (p, ms) :: rest =>
        if p == modPrefix then (p, mod :: ms) :: rest
        else (p, ms) :: updateOrAdd rest
    updateOrAdd acc

  let items := grouped.map fun (modPrefix, mods) =>
    if modPrefix.isEmpty then
      Html.element "li" false #[] <| mods.reverse.toArray.map fun m =>
        Html.element "a" true #[("href", s!"modules/{m.replace "." "_"}.html")] #[Html.text m]
    else
      Html.element "li" false #[("class", "nav-group")] #[
        Html.element "span" true #[("class", "nav-toggle")] #[Html.text modPrefix],
        Html.element "ul" false #[] <| mods.reverse.toArray.map fun m =>
          Html.element "li" false #[] #[
            Html.element "a" true #[("href", s!"modules/{m.replace "." "_"}.html")] #[Html.text m]
          ]
      ]

  return Html.element "ul" false #[("class", "nav-tree")] items.toArray

/-! ## Page Generators -/

/-- Statistics for verification summary -/
structure VerificationStats where
  totalDecls : Nat
  theorems : Nat
  provedTheorems : Nat  -- theorems without sorry
  axiomTheorems : Nat   -- theorems using axioms
  sorryTheorems : Nat   -- theorems with sorry
  definitions : Nat
  sorryDefinitions : Nat
  totalModules : Nat
  -- Four-Category Ontology (Definition Categories)
  mathAbstractions : Nat := 0
  compDatatypes : Nat := 0
  mathDefinitions : Nat := 0
  compOperations : Nat := 0
  -- Theorem Taxonomy
  computationalTheorems : Nat := 0
  mathematicalTheorems : Nat := 0
  bridgingTheorems : Nat := 0
  soundnessTheorems : Nat := 0
  completenessTheorems : Nat := 0
  unclassifiedTheorems : Nat := 0
  deriving Repr, Inhabited, ToJson, FromJson

/-- Compute verification statistics from entries -/
def computeStats (entries : NameMap APIMeta) : VerificationStats := Id.run do
  let mut stats : VerificationStats := {
    totalDecls := entries.size
    theorems := 0
    provedTheorems := 0
    axiomTheorems := 0
    sorryTheorems := 0
    definitions := 0
    sorryDefinitions := 0
    totalModules := 0
    mathAbstractions := 0
    compDatatypes := 0
    mathDefinitions := 0
    compOperations := 0
    computationalTheorems := 0
    mathematicalTheorems := 0
    bridgingTheorems := 0
    soundnessTheorems := 0
    completenessTheorems := 0
    unclassifiedTheorems := 0
  }
  let mut modules : Std.HashSet Name := {}

  for (_, apiMeta) in entries.toList do
    modules := modules.insert apiMeta.module
    match apiMeta.kind with
    | .apiTheorem thmData =>
      stats := { stats with theorems := stats.theorems + 1 }
      if apiMeta.hasSorry then
        stats := { stats with sorryTheorems := stats.sorryTheorems + 1 }
      else if apiMeta.usesAxioms then
        stats := { stats with axiomTheorems := stats.axiomTheorems + 1 }
      else
        stats := { stats with provedTheorems := stats.provedTheorems + 1 }
      -- Count by theorem kind
      match thmData.kind with
      | some .computationalProperty =>
        stats := { stats with computationalTheorems := stats.computationalTheorems + 1 }
      | some .mathematicalProperty =>
        stats := { stats with mathematicalTheorems := stats.mathematicalTheorems + 1 }
      | some .bridgingProperty =>
        stats := { stats with bridgingTheorems := stats.bridgingTheorems + 1 }
      | some .soundnessProperty =>
        stats := { stats with soundnessTheorems := stats.soundnessTheorems + 1 }
      | some .completenessProperty =>
        stats := { stats with completenessTheorems := stats.completenessTheorems + 1 }
      | none =>
        stats := { stats with unclassifiedTheorems := stats.unclassifiedTheorems + 1 }
    | .apiDef defData =>
      stats := { stats with definitions := stats.definitions + 1 }
      if apiMeta.hasSorry then
        stats := { stats with sorryDefinitions := stats.sorryDefinitions + 1 }
      -- Count by definition category
      match defData.category with
      | .mathematicalDefinition =>
        stats := { stats with mathDefinitions := stats.mathDefinitions + 1 }
      | .computationalOperation =>
        stats := { stats with compOperations := stats.compOperations + 1 }
    | .apiType typeCategory =>
      -- Count types by category
      match typeCategory with
      | .mathematicalAbstraction =>
        stats := { stats with mathAbstractions := stats.mathAbstractions + 1 }
      | .computationalDatatype =>
        stats := { stats with compDatatypes := stats.compDatatypes + 1 }

  { stats with totalModules := modules.size }

/-- Format a file size in human-readable format (KB, MB, GB) -/
def formatFileSize (bytes : UInt64) : String :=
  if bytes < 1024 then
    s!"{bytes} B"
  else if bytes < 1024 * 1024 then
    let kb := bytes.toNat / 1024
    s!"{kb} KB"
  else if bytes < 1024 * 1024 * 1024 then
    let mb := bytes.toNat / (1024 * 1024)
    let kb := (bytes.toNat % (1024 * 1024)) / 1024
    if kb >= 100 then s!"{mb}.{kb / 100} MB" else s!"{mb} MB"
  else
    let gb := bytes.toNat / (1024 * 1024 * 1024)
    s!"{gb} GB"

/-- Get file size if file exists -/
def getFileSizeOpt (path : System.FilePath) : IO (Option UInt64) := do
  if ← path.pathExists then
    let fileMeta ← path.metadata
    return some fileMeta.byteSize
  else
    return none

/-- Generate downloads section for index page (IO because it needs file system access) -/
def generateDownloadsSectionIO (dataDir : System.FilePath) : IO Html := do
  -- Check which files exist and get their sizes
  let jsonPath := dataDir / "classification-cache.json"
  let jsonlPath := dataDir / "classification-cache.jsonl"
  let yamlPath := dataDir / "commands.yaml"

  let jsonSize ← getFileSizeOpt jsonPath
  let jsonlSize ← getFileSizeOpt jsonlPath
  let yamlSize ← getFileSizeOpt yamlPath

  -- Build list of download items
  let mut items : Array Html := #[]

  if let some size := jsonSize then
    items := items.push <| Html.element "li" false #[] #[
      Html.element "a" true #[("href", "../classification-cache.json"), ("download", "")] #[
        Html.text "📄 classification-cache.json"
      ],
      Html.text s!" ({formatFileSize size}) — Classification metadata"
    ]

  if let some size := jsonlSize then
    items := items.push <| Html.element "li" false #[] #[
      Html.element "a" true #[("href", "../classification-cache.jsonl"), ("download", "")] #[
        Html.text "📄 classification-cache.jsonl"
      ],
      Html.text s!" ({formatFileSize size}) — Full classification data",
      Html.element "br" true #[] #[],
      Html.element "small" true #[("style", "color: #888;")] #[
        Html.text "⚠️ Note: This is a Git LFS file; download may be large"
      ]
    ]

  if let some size := yamlSize then
    items := items.push <| Html.element "li" false #[] #[
      Html.element "a" true #[("href", "../commands.yaml"), ("download", "")] #[
        Html.text "📋 commands.yaml"
      ],
      Html.text s!" ({formatFileSize size}) — Build commands log"
    ]

  if items.isEmpty then
    return Html.text ""
  else
    return Html.element "section" false #[("class", "downloads")] #[
      Html.element "h2" true #[] #[Html.text "Downloads"],
      Html.element "p" true #[] #[Html.text "Data files for this project:"],
      Html.element "ul" false #[] items
    ]

/-- Generate project info section showing config.toml settings -/
def generateProjectInfoSection (cfg : StaticHtmlConfig) : Html :=
  -- Only show if we have project info
  if cfg.projectDescription.isNone && cfg.projectModules.isEmpty && cfg.projectSettings.isEmpty then
    Html.text ""
  else
    let descHtml := match cfg.projectDescription with
      | some desc => Html.element "p" true #[] #[Html.text desc]
      | none => Html.text ""

    let modulesHtml := if cfg.projectModules.isEmpty then Html.text ""
      else Html.element "p" true #[] #[
        Html.element "strong" true #[] #[Html.text "Modules: "],
        Html.text (String.intercalate ", " cfg.projectModules.toList)
      ]

    let repoHtml := Html.element "p" true #[] #[
      Html.element "strong" true #[] #[Html.text "Repository: "],
      Html.element "a" true #[("href", cfg.repoUrl), ("target", "_blank")] #[Html.text cfg.repoUrl]
    ]

    let settingsHtml := if cfg.projectSettings.isEmpty then Html.text ""
      else
        let settingItems := cfg.projectSettings.map fun (k, v) =>
          Html.element "li" false #[] #[
            Html.element "code" true #[] #[Html.text k],
            Html.text ": ",
            Html.element "code" true #[] #[Html.text v]
          ]
        Html.element "details" false #[] #[
          Html.element "summary" true #[("style", "cursor: pointer; color: #4ecdc4;")] #[
            Html.text "Analysis settings"
          ],
          Html.element "ul" false #[("style", "margin-top: 10px;")] settingItems
        ]

    Html.element "section" false #[("class", "project-info")] #[
      Html.element "h2" true #[] #[Html.text "Project Info"],
      descHtml,
      repoHtml,
      modulesHtml,
      settingsHtml
    ]

/-- Generate the main index page. The optional downloadsSection should be pre-computed via IO. -/
def generateIndexPage (stats : VerificationStats) (cfg : StaticHtmlConfig)
    (downloadsSection : Option Html := none) : BaseHtmlM Html := do
  let provedPct := if stats.theorems > 0
    then (stats.provedTheorems * 100) / stats.theorems
    else 0
  let axiomPct := if stats.theorems > 0
    then (stats.axiomTheorems * 100) / stats.theorems
    else 0
  let sorryPct := if stats.theorems > 0
    then (stats.sorryTheorems * 100) / stats.theorems
    else 0

  -- Helper to compute percentage
  let pct := fun (n : Nat) (total : Nat) => if total > 0 then (n * 100) / total else 0

  -- Total types = mathAbstractions + compDatatypes
  let totalTypes := stats.mathAbstractions + stats.compDatatypes

  let content := Html.element "div" false #[("class", "container")] #[
    Html.element "h1" true #[] #[Html.text "Verification Overview"],

    Html.element "section" false #[("class", "stats-summary")] #[
      Html.element "h2" true #[] #[Html.text "Summary"],
      Html.element "table" false #[] #[
        Html.element "tr" false #[] #[
          Html.element "th" true #[] #[Html.text "Metric"],
          Html.element "th" true #[] #[Html.text "Count"],
          Html.element "th" true #[] #[Html.text "Percentage"]
        ],
        Html.element "tr" false #[] #[
          Html.element "td" true #[] #[Html.text "Total Declarations"],
          Html.element "td" true #[] #[Html.text (toString stats.totalDecls)],
          Html.element "td" true #[] #[Html.text "-"]
        ],
        Html.element "tr" false #[] #[
          Html.element "td" true #[] #[Html.text "Total Modules"],
          Html.element "td" true #[] #[Html.text (toString stats.totalModules)],
          Html.element "td" true #[] #[Html.text "-"]
        ],
        Html.element "tr" false #[] #[
          Html.element "td" true #[] #[Html.text "Theorems (Total)"],
          Html.element "td" true #[] #[Html.text (toString stats.theorems)],
          Html.element "td" true #[] #[Html.text "-"]
        ],
        Html.element "tr" false #[] #[
          Html.element "td" true #[("class", "indent")] #[Html.text "✓ Fully Proved"],
          Html.element "td" true #[] #[Html.text (toString stats.provedTheorems)],
          Html.element "td" true #[] #[Html.text s!"{provedPct}%"]
        ],
        Html.element "tr" false #[] #[
          Html.element "td" true #[("class", "indent")] #[Html.text "⚠ Uses Axioms"],
          Html.element "td" true #[] #[Html.text (toString stats.axiomTheorems)],
          Html.element "td" true #[] #[Html.text s!"{axiomPct}%"]
        ],
        Html.element "tr" false #[] #[
          Html.element "td" true #[("class", "indent")] #[Html.text "✗ Has Sorry"],
          Html.element "td" true #[] #[Html.text (toString stats.sorryTheorems)],
          Html.element "td" true #[] #[Html.text s!"{sorryPct}%"]
        ],
        Html.element "tr" false #[] #[
          Html.element "td" true #[] #[Html.text "Definitions with Sorry"],
          Html.element "td" true #[] #[Html.text (toString stats.sorryDefinitions)],
          Html.element "td" true #[] #[Html.text "-"]
        ]
      ]
    ],

    Html.element "section" false #[("class", "progress-section")] #[
      Html.element "h2" true #[] #[Html.text "Theorem Verification Progress"],
      Html.element "div" false #[("class", "progress-bar")] #[
        Html.element "div" true #[("class", "progress-fill progress-proved"), ("style", s!"width: {provedPct}%")] #[],
        Html.element "div" true #[("class", "progress-fill progress-axiom"), ("style", s!"width: {axiomPct}%")] #[],
        Html.element "div" true #[("class", "progress-fill progress-sorry"), ("style", s!"width: {sorryPct}%")] #[]
      ],
      Html.element "div" false #[("class", "progress-legend")] #[
        Html.element "span" true #[] #[
          Html.element "span" true #[("class", "badge badge-proved")] #[Html.text "Proved"],
          Html.text s!" {stats.provedTheorems}"
        ],
        Html.element "span" true #[] #[
          Html.element "span" true #[("class", "badge badge-axiom")] #[Html.text "Axiom"],
          Html.text s!" {stats.axiomTheorems}"
        ],
        Html.element "span" true #[] #[
          Html.element "span" true #[("class", "badge badge-sorry")] #[Html.text "Sorry"],
          Html.text s!" {stats.sorryTheorems}"
        ]
      ]
    ],

    -- Four-Category Ontology Table
    Html.element "section" false #[("class", "ontology-section")] #[
      Html.element "h2" true #[] #[Html.text "Four-Category Ontology"],
      Html.element "p" true #[] #[Html.text "Classification of declarations based on E.J. Lowe's metaphysical framework."],
      Html.element "table" false #[("class", "ontology-table")] #[
        Html.element "thead" false #[] #[
          Html.element "tr" false #[] #[
            Html.element "th" true #[] #[Html.text ""],
            Html.element "th" true #[] #[Html.text "Mathematical (Prop)"],
            Html.element "th" true #[] #[Html.text "Computational (Data)"]
          ]
        ],
        Html.element "tbody" false #[] #[
          Html.element "tr" false #[] #[
            Html.element "th" true #[] #[Html.text "Substantial (Types)"],
            Html.element "td" true #[] #[Html.text s!"{stats.mathAbstractions} ({pct stats.mathAbstractions totalTypes}%)"],
            Html.element "td" true #[] #[Html.text s!"{stats.compDatatypes} ({pct stats.compDatatypes totalTypes}%)"]
          ],
          Html.element "tr" false #[] #[
            Html.element "th" true #[] #[Html.text "Non-substantial (Defs)"],
            Html.element "td" true #[] #[Html.text s!"{stats.mathDefinitions} ({pct stats.mathDefinitions stats.definitions}%)"],
            Html.element "td" true #[] #[Html.text s!"{stats.compOperations} ({pct stats.compOperations stats.definitions}%)"]
          ]
        ]
      ]
    ],

    -- Theorem Taxonomy Table
    Html.element "section" false #[("class", "taxonomy-section")] #[
      Html.element "h2" true #[] #[Html.text "Theorem Taxonomy"],
      Html.element "p" true #[] #[Html.text "Classification of theorems by what they prove."],
      Html.element "table" false #[] #[
        Html.element "thead" false #[] #[
          Html.element "tr" false #[] #[
            Html.element "th" true #[] #[Html.text "Theorem Kind"],
            Html.element "th" true #[] #[Html.text "Count"],
            Html.element "th" true #[] #[Html.text "Percentage"]
          ]
        ],
        Html.element "tbody" false #[] #[
          Html.element "tr" false #[] #[
            Html.element "td" true #[] #[Html.text "mathematicalProperty"],
            Html.element "td" true #[] #[Html.text (toString stats.mathematicalTheorems)],
            Html.element "td" true #[] #[Html.text s!"{pct stats.mathematicalTheorems stats.theorems}%"]
          ],
          Html.element "tr" false #[] #[
            Html.element "td" true #[] #[Html.text "bridgingProperty"],
            Html.element "td" true #[] #[Html.text (toString stats.bridgingTheorems)],
            Html.element "td" true #[] #[Html.text s!"{pct stats.bridgingTheorems stats.theorems}%"]
          ],
          Html.element "tr" false #[] #[
            Html.element "td" true #[] #[Html.text "computationalProperty"],
            Html.element "td" true #[] #[Html.text (toString stats.computationalTheorems)],
            Html.element "td" true #[] #[Html.text s!"{pct stats.computationalTheorems stats.theorems}%"]
          ],
          Html.element "tr" false #[] #[
            Html.element "td" true #[] #[Html.text "soundnessProperty"],
            Html.element "td" true #[] #[Html.text (toString stats.soundnessTheorems)],
            Html.element "td" true #[] #[Html.text s!"{pct stats.soundnessTheorems stats.theorems}%"]
          ],
          Html.element "tr" false #[] #[
            Html.element "td" true #[] #[Html.text "completenessProperty"],
            Html.element "td" true #[] #[Html.text (toString stats.completenessTheorems)],
            Html.element "td" true #[] #[Html.text s!"{pct stats.completenessTheorems stats.theorems}%"]
          ],
          Html.element "tr" false #[] #[
            Html.element "td" true #[("class", "muted")] #[Html.text "unclassified"],
            Html.element "td" true #[("class", "muted")] #[Html.text (toString stats.unclassifiedTheorems)],
            Html.element "td" true #[("class", "muted")] #[Html.text s!"{pct stats.unclassifiedTheorems stats.theorems}%"]
          ]
        ]
      ]
    ],

    Html.element "section" false #[] #[
      Html.element "h2" true #[] #[Html.text "Quick Links"],
      Html.element "ul" false #[] #[
        Html.element "li" false #[] #[Html.element "a" true #[("href", "modules/index.html")] #[Html.text "Browse by Module"]],
        Html.element "li" false #[] #[Html.element "a" true #[("href", "search.html")] #[Html.text "Search Declarations"]],
        Html.element "li" false #[] #[Html.element "a" true #[("href", s!"{cfg.apiBaseUrl}/index.html")] #[Html.text "API Documentation"]]
      ]
    ]
  ]

  -- Build full content with project info and downloads sections
  let projectInfoHtml := generateProjectInfoSection cfg
  let sectionsHtml := match downloadsSection with
    | some ds => Html.element "div" false #[] #[projectInfoHtml, ds]
    | none => projectInfoHtml

  let fullContent := Html.element "div" false #[] #[content, sectionsHtml]

  baseHtml "Overview" fullContent cfg

/-- Generate the search page -/
def generateSearchPage (cfg : StaticHtmlConfig) : BaseHtmlM Html := do
  let content := Html.element "div" false #[("class", "container")] #[
    Html.element "h1" true #[] #[Html.text "Search Declarations"],
    Html.element "input" true #[("type", "text"), ("id", "search-input"), ("placeholder", "Search by name or module...")] #[],
    Html.element "div" true #[("id", "search-results")] #[]
  ]

  baseHtml "Search" content cfg

/-- Generate module index page -/
def generateModuleIndexPage (moduleReports : Array (String × String × ModuleStats)) (cfg : StaticHtmlConfig) : BaseHtmlM Html := do
  -- Sort modules alphabetically by file path
  let sortedReports := moduleReports.qsort fun (_, _, s1) (_, _, s2) => decide (s1.filePath < s2.filePath)

  let rows := sortedReports.map fun (safeFilename, _, stats) =>
    let totalSorry := stats.defsWithSorry + stats.theoremsWithSorry
    let sorryBadge := if totalSorry > 0
      then Html.element "span" true #[("class", "badge badge-sorry")] #[Html.text (toString totalSorry)]
      else Html.text ""
    Html.element "tr" false #[] #[
      Html.element "td" false #[] #[
        Html.element "a" true #[("href", s!"{safeFilename}.html")] #[Html.text stats.filePath]
      ],
      Html.element "td" false #[] #[Html.text (toString stats.totalDefs)],
      Html.element "td" false #[] #[Html.text (toString stats.totalTheorems)],
      Html.element "td" false #[] #[sorryBadge]
    ]

  let content := Html.element "div" false #[("class", "container")] #[
    Html.element "h1" true #[] #[Html.text "Modules"],
    Html.element "p" true #[] #[Html.text s!"{sortedReports.size} modules"],
    Html.element "table" false #[] #[
      Html.element "thead" false #[] #[
        Html.element "tr" false #[] #[
          Html.element "th" true #[] #[Html.text "Module"],
          Html.element "th" true #[] #[Html.text "Definitions"],
          Html.element "th" true #[] #[Html.text "Theorems"],
          Html.element "th" true #[] #[Html.text "Sorry"]
        ]
      ],
      Html.element "tbody" false #[] rows
    ]
  ]

  -- depthToRoot = 1 because modules/index.html is one level deep
  baseHtml "Modules" content cfg [("Verification", "../index.html")] 1

/-- Declaration entry for search index -/
structure DeclIndexEntry where
  name : String
  module : String
  kind : String
  status : String  -- "proved", "axiom", "sorry", "def"
  deriving ToJson, FromJson

/-- Generate search index JSON -/
def generateSearchIndex (entries : NameMap APIMeta) : String :=
  let declEntries := entries.toList.map fun (name, apiMeta) =>
    let status :=
      if apiMeta.hasSorry then "sorry"
      else if apiMeta.usesAxioms then "axiom"
      else match apiMeta.kind with
        | .apiTheorem _ => "proved"
        | _ => "def"
    { name := name.toString
      module := apiMeta.module.toString
      kind := apiMeta.categoryString
      status := status : DeclIndexEntry }

  (Lean.toJson declEntries).compress

/-! ## Running BaseHtmlM -/

/-- Create a minimal SiteBaseContext for running BaseHtmlM outside doc-gen4's full pipeline -/
def defaultSiteBaseContext (buildDir : System.FilePath := ".") : SiteBaseContext where
  buildDir := buildDir
  hierarchy := default  -- Empty hierarchy
  depthToRoot := 0
  currentName := none
  refs := #[]

/-- Run a BaseHtmlM computation with a default context -/
def runBaseHtmlM (m : BaseHtmlM α) (buildDir : System.FilePath := ".") : α :=
  m.run (defaultSiteBaseContext buildDir)

/-! ## Main Generation Pipeline -/

/-- Split an array into chunks of the given size -/
private partial def chunkArray {α : Type} (arr : Array α) (chunkSize : Nat) : Array (Array α) :=
  if chunkSize == 0 then #[arr]
  else go 0 #[]
where
  go (i : Nat) (acc : Array (Array α)) : Array (Array α) :=
    if i >= arr.size then acc
    else
      let endIdx := min (i + chunkSize) arr.size
      go endIdx (acc.push (arr.extract i endIdx))

/-- Wrap markdown content in HTML page structure -/
private def wrapModuleHtml (moduleName : String) (markdownContent : String) (cfg : StaticHtmlConfig) : String :=
  let htmlBody := markdownToHtml markdownContent
  s!"<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
  <title>{moduleName} - {cfg.projectName} Verification</title>
  <link rel=\"stylesheet\" href=\"../style.css\">
  <script defer src=\"../verification.js\"></script>
</head>
<body>
  <header>
    <h1>{cfg.projectName} Verification Report</h1>
    <nav>
      <a href=\"../index.html\">Overview</a>
      <a href=\"index.html\">Modules</a>
      <a href=\"../search.html\">Search</a>
      <a href=\"../{cfg.apiBaseUrl}/index.html\">API Docs</a>
    </nav>
  </header>
  <main>
    <div class=\"container\">
      <nav class=\"breadcrumbs\">
        <a href=\"../index.html\">Verification</a> /
        <a href=\"index.html\">Modules</a> /
        {moduleName}
      </nav>
      {htmlBody}
    </div>
  </main>
</body>
</html>"

/-- Generate all static HTML files -/
def generateStaticSite (cfg : StaticHtmlConfig)
    (entries : NameMap APIMeta)
    (moduleReports : Array (String × String × ModuleStats))
    : IO Unit := do
  let startTime ← IO.monoMsNow

  -- Create output directories
  IO.FS.createDirAll cfg.outputDir
  IO.FS.createDirAll (cfg.outputDir / "modules")

  -- Write static assets
  IO.println s!"  Writing static assets..."
  IO.FS.writeFile (cfg.outputDir / "style.css") verificationCss
  IO.FS.writeFile (cfg.outputDir / "verification.js") verificationJs

  -- Compute stats
  let stats := computeStats entries

  -- Write stats.json for programmatic access (used by experiments pipeline)
  IO.FS.writeFile (cfg.outputDir / "stats.json") (Lean.toJson stats).compress

  -- Generate downloads section (needs IO for file sizes)
  let downloadsSection ← match cfg.dataFilesDir with
    | some dataDir => some <$> generateDownloadsSectionIO dataDir
    | none => pure none

  -- Generate and write index page
  IO.println s!"  Generating index page..."
  let indexHtml := runBaseHtmlM (generateIndexPage stats cfg downloadsSection) cfg.outputDir
  IO.FS.writeFile (cfg.outputDir / "index.html") indexHtml.toString

  -- Generate search page
  let searchHtml := runBaseHtmlM (generateSearchPage cfg) cfg.outputDir
  IO.FS.writeFile (cfg.outputDir / "search.html") searchHtml.toString

  -- Generate search index
  IO.println s!"  Generating search index..."
  let searchIndex := generateSearchIndex entries
  IO.FS.writeFile (cfg.outputDir / "declarations.json") searchIndex

  -- Generate module index
  let moduleIndexHtml := runBaseHtmlM (generateModuleIndexPage moduleReports cfg) cfg.outputDir
  IO.FS.writeFile (cfg.outputDir / "modules" / "index.html") moduleIndexHtml.toString

  -- Generate individual module pages (parallel)
  IO.println s!"  Generating {moduleReports.size} module pages ({cfg.workers} workers)..."
  let moduleStartTime ← IO.monoMsNow

  if cfg.workers > 0 && moduleReports.size > cfg.workers then
    -- Parallel generation
    let chunkSize := (moduleReports.size + cfg.workers - 1) / cfg.workers
    let chunks := chunkArray moduleReports chunkSize

    let tasks ← chunks.mapM fun chunk => do
      IO.asTask (prio := .dedicated) do
        for (safeFilename, markdownContent, _) in chunk do
          let htmlContent := wrapModuleHtml safeFilename markdownContent cfg
          IO.FS.writeFile (cfg.outputDir / "modules" / s!"{safeFilename}.html") htmlContent

    -- Wait for all tasks
    for task in tasks do
      let _ ← IO.ofExcept (← IO.wait task)
  else
    -- Sequential generation
    for (safeFilename, markdownContent, _) in moduleReports do
      let htmlContent := wrapModuleHtml safeFilename markdownContent cfg
      IO.FS.writeFile (cfg.outputDir / "modules" / s!"{safeFilename}.html") htmlContent

  let moduleEndTime ← IO.monoMsNow
  let moduleDuration := formatDurationMs (moduleEndTime - moduleStartTime)
  IO.println s!"  Module pages generated ({moduleDuration})"

  let endTime ← IO.monoMsNow
  let totalDuration := formatDurationMs (endTime - startTime)
  IO.println s!"  Static site generated at {cfg.outputDir}/ ({totalDuration})"

/-! ## Dependency Stub Generation

doc-gen4 generates links to external dependencies (Batteries, Init, Std, etc.) that aren't
included in the project's documentation. We create stub pages for these to avoid broken links.
-/

/-- Common Lean dependencies that doc-gen4 may link to but aren't included in project docs -/
private def commonDependencies : List String := ["Batteries", "Init", "Std", "Lean", "Lake"]

/-- CSS for dependency stub pages (separate to avoid string interpolation issues) -/
private def dependencyStubCss : String :=
  "body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; " ++
  "max-width: 600px; margin: 100px auto; padding: 20px; text-align: center; } " ++
  "h1 { color: #333; } " ++
  "p { color: #666; line-height: 1.6; } " ++
  "a { color: #0366d6; } " ++
  ".back { margin-top: 30px; }"

/-- Generate a stub HTML page for a missing dependency module -/
private def generateDependencyStub (name : String) (projectName : String) : String :=
  "<!DOCTYPE html>\n<html>\n<head>\n" ++
  "  <meta charset=\"UTF-8\">\n" ++
  s!"  <title>{name} - External Dependency</title>\n" ++
  s!"  <style>{dependencyStubCss}</style>\n" ++
  "</head>\n<body>\n" ++
  s!"  <h1>{name}</h1>\n" ++
  s!"  <p><strong>{name}</strong> is a dependency of {projectName}, but its documentation is not included in this site.</p>\n" ++
  "  <p>You can find the official documentation at:</p>\n" ++
  "  <ul style=\"list-style: none; padding: 0;\">\n" ++
  s!"    <li><a href=\"https://leanprover-community.github.io/mathlib4_docs/{name}.html\">Mathlib4 Docs</a> (if part of Mathlib)</li>\n" ++
  s!"    <li><a href=\"https://leanprover-community.github.io/batteries/docs/{name}.html\">Batteries Docs</a> (if part of Batteries)</li>\n" ++
  "  </ul>\n" ++
  s!"  <p class=\"back\"><a href=\"index.html\">← Back to {projectName} documentation</a></p>\n" ++
  "</body>\n</html>"

/-- Create stub HTML pages for missing dependency modules that doc-gen4 links to -/
def createMissingDependencyStubs (apiDir : System.FilePath) (projectName : String) : IO Unit := do
  let mut created := #[]
  for dep in commonDependencies do
    let stubPath := apiDir / s!"{dep}.html"
    if !(← stubPath.pathExists) then
      IO.FS.writeFile stubPath (generateDependencyStub dep projectName)
      created := created.push dep
  if created.size > 0 then
    IO.println s!"  Created stub pages for missing dependencies: {created.toList}"

end DocVerificationBridge.StaticHtml
