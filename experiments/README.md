# doc-verification-bridge Experiments

This directory contains the experimental pipeline for evaluating doc-verification-bridge across multiple Lean 4 projects.

## Overview

The pipeline (implemented in Lean 4):
1. Clones repositories from the configured list
2. Auto-detects the git branch (or uses configured value)
3. Sets up a `docvb` subdirectory with proper lakefile
4. Builds each project
5. Runs `unified-doc` with the configured classification mode (auto or annotated)
6. Generates per-module verification reports with source and API links
7. Detects `sorry` in definitions and theorems
8. Generates a meta-summary page with sortable statistics

## Quick Start

```bash
# Run the full experiment pipeline (fresh start)
./run.sh run

# Resume after interruption (skips completed projects)
./run.sh run --resume

# Update all projects (git pull + regenerate docs)
./run.sh run --update

# Regenerate summary page from existing coverage data (no rebuild)
./run.sh refresh

# Refresh summary and serve the results
./run.sh serve

# Clean up all artifacts
./run.sh clean
```

Or use the Lean executable directly:

```bash
# From the doc-verification-bridge root directory
lake build experiments
.lake/build/bin/experiments run --config experiments/config.toml
.lake/build/bin/experiments run --resume --config experiments/config.toml
.lake/build/bin/experiments run --update --config experiments/config.toml
.lake/build/bin/experiments refresh --config experiments/config.toml
.lake/build/bin/experiments serve --config experiments/config.toml
```

## Configuration

Edit `config.toml` to:
- Add/remove projects
- Configure global default settings
- Set classification mode per project
- Override global settings per-project

All paths in `config.toml` are relative to the experiments directory.

### Global Settings

The `[settings]` section defines global defaults that apply to all projects unless overridden:

```toml
[settings]
doc_verification_bridge_path = ".."
repos_dir = "repos"
sites_dir = "sites"
base_port = 9000
max_parallel_jobs = 8

# Global defaults for per-project settings
lake_exe_cache_get = false    # Run lake exe cache get
disable_equations = false     # Disable equation generation
skip_proof_deps = false       # Skip proof dependency extraction
proof_dep_workers = 0         # Parallel proof extraction workers
html_workers = 0              # Parallel HTML generation workers
slow_threshold_secs = 30      # Warn about slow theorems during proof dep extraction
```

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| `lake_exe_cache_get` | bool | `false` | Run `lake exe cache get` before build |
| `disable_equations` | bool | `false` | Disable equation generation (avoids timeouts) |
| `skip_proof_deps` | bool | `false` | Skip proof dependency extraction entirely |
| `proof_dep_workers` | int | `0` | Parallel workers for proof extraction (0 = sequential) |
| `html_workers` | int | `0` | Parallel workers for HTML generation (0 = sequential) |
| `slow_threshold_secs` | int | `30` | Seconds before warning about slow theorems during proof dep extraction |

### Adding a New Project

```toml
[[projects]]
name = "my-project"
repo = "https://github.com/user/my-project"
modules = ["MyProject"]
description = "Description for the summary page"
classification_mode = "auto"  # or "annotated"
```

### Project Configuration Options

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `name` | string | required | Project identifier (used for directory names) |
| `repo` | string | required | Git repository URL |
| `modules` | array | required | Top-level module name(s) to analyze |
| `description` | string | `""` | Description shown on summary page |
| `classification_mode` | string | `"auto"` | `"auto"` or `"annotated"` |
| `subdirectory` | string | none | Subdirectory for monorepos |
| `branch` | string | auto-detect | Git branch (auto-detected if not specified) |
| `lake_exe_cache_get` | bool | global | Run `lake exe cache get` (overrides global) |
| `disable_equations` | bool | global | Disable equation generation (overrides global) |
| `skip_proof_deps` | bool | global | Skip proof dependency extraction (overrides global) |
| `proof_dep_workers` | int | global | Parallel proof extraction workers (overrides global) |
| `proof_dep_blacklist` | array | `[]` | Theorem names to skip during proof dep extraction |
| `html_workers` | int | global | Parallel HTML generation workers (overrides global) |

> **Note:** Per-project settings override global defaults. If a project doesn't specify a value, the global setting is used.

### Monorepo Support

For repositories where the Lean project is in a subdirectory:

```toml
[[projects]]
name = "mm0"
repo = "https://github.com/digama0/mm0"
modules = ["MM0"]
subdirectory = "mm0-lean4"  # Lean 4 project is in this subdirectory
description = "Metamath Zero proof language"
```

### Branch Auto-Detection

The git branch is automatically detected from the cloned repository. This handles repos that use `master` instead of `main`. You can override this if needed:

```toml
[[projects]]
name = "legacy-project"
repo = "https://github.com/user/legacy-project"
modules = ["Legacy"]
branch = "develop"  # Override auto-detection
```

### Classification Modes

Each project can specify its classification mode:

| Mode | Value | Description |
|------|-------|-------------|
| **Auto** | `"auto"` | Automatic heuristic-based classification (default) |
| **Annotated** | `"annotated"` | Only classify declarations with explicit `@[api_type]`, `@[api_def]`, `@[api_theorem]` annotations |

### Performance Options for Large Projects

For very large projects like mathlib4, proof dependency extraction can be slow. You can configure these either globally (in `[settings]`) or per-project:

| Option | Effect | Trade-off |
|--------|--------|-----------|
| `skip_proof_deps = true` | Skips proof dependency extraction entirely | Fast, but no `dependsOn` data in reports |
| `proof_dep_workers = N` | Parallel proof extraction with up to N workers | Full data with better throughput |
| `html_workers = N` | Parallel HTML file generation | Faster output for large projects |
### Handling Slow Theorems

Some theorems have very large proof terms that take a long time to analyze. The pipeline provides two mechanisms:

1. **Slow theorem warnings**: When a theorem takes longer than `slow_threshold_secs` (default: 30s), a warning is printed:
   ```
   [PhysLean] ⏱️ SLOW: Worker 3 has spent 35s on `PhysLean.SomeSlowTheorem`
   ```

2. **Per-project blacklist**: Add slow theorems to the blacklist to skip them in future runs:
   ```toml
   [[projects]]
   name = "PhysLean"
   proof_dep_blacklist = ["PhysLean.SlowTheorem1", "PhysLean.SlowTheorem2"]
   ```

The blacklist uses exact theorem name matching (fully qualified names).
**Example for mathlib4 (project-specific overrides):**

```toml
[[projects]]
name = "mathlib4"
repo = "https://github.com/leanprover-community/mathlib4"
modules = ["Mathlib"]
lake_exe_cache_get = true     # Use cache for faster build
disable_equations = true       # Avoid timeout on complex proofs
proof_dep_workers = 50         # Parallel proof dep extraction
html_workers = 20              # Parallel HTML file generation
```

**Or set global defaults for all projects:**

```toml
[settings]
# ... other settings ...
proof_dep_workers = 8          # Default for all projects
html_workers = 10              # Default for all projects
```

The `proof_dep_workers` option uses a two-phase classification:
1. **Phase 1 (Sequential)**: Extract type information and light annotations in MetaM
2. **Phase 2 (Parallel)**: Extract proof dependencies using worker threads

## Run Modes

The experiment runner supports three modes for different workflows:

### Fresh Run (default)
```bash
./run.sh run
```
- Deletes all existing `repos/` and `sites/` data for each project
- Clones all repositories fresh
- Rebuilds everything from scratch

### Resume Mode
```bash
./run.sh run --resume
```
- **Skips** projects marked as `completed`
- **Restarts** projects marked as `in-progress` or `failed` (cleans and re-runs)
- **Processes** projects not yet started
- Useful after an interruption (Ctrl+C) to continue where you left off

### Update Mode
```bash
./run.sh run --update
```
- Runs `git pull` on existing repositories (instead of fresh clone)
- Deletes only the `sites/<project>/` directory (keeps repos)
- Regenerates documentation with latest code
- Useful for periodic updates without full re-clone

### Reanalyze Mode
```bash
./run.sh run --reanalyze
```
- Skips git clone/pull AND project build (requires existing build)
- Re-runs full unified-doc pipeline: doc-gen4 + classification
- Useful when updating doc-verification-bridge itself (no project changes)

### Reclassify Mode
```bash
./run.sh run --reclassify
```
- Skips git clone/pull, project build, AND doc-gen4 generation
- Only re-runs classification and report generation
- Requires existing doc-gen4 output in `api-temp/` directory
- **Fastest option** for iterating on classification logic
- Useful when classification code changes but API docs don't need regeneration

### HTML-Only Mode
```bash
./run.sh run --html-only
```
- Loads classification from cache file (requires prior run with caching)
- Skips git, build, doc-gen4, AND classification phases
- Only regenerates HTML files and builds the site
- **Fastest option** for iterating on report templates and styling
- Requires `--load-classification` or cached classification from previous run

### State Tracking

Each project's state is tracked in `sites/<project>/.state`:

| State | Meaning |
|-------|---------|
| `not-started` | Project hasn't been processed yet |
| `in-progress` | Currently processing (set at start) |
| `completed` | Successfully finished |
| `failed` | Processing encountered an error |

## Output Structure

```
experiments/
├── config.toml           # Configuration
├── run.sh                # Convenience wrapper
├── repos/                # Cloned repositories
│   ├── batteries/
│   ├── mathlib4/
│   └── ...
├── sites/                # Generated documentation
│   ├── index.html        # Meta-summary page
│   ├── batteries/
│   │   ├── .state        # State file
│   │   ├── commands.yaml # Command execution log
│   │   ├── index.html    # Status/error page (for incomplete/failed)
│   │   └── site/         # Static HTML output (for successful builds)
│   │       ├── index.html
│   │       ├── api/           # doc-gen4 API documentation
│   │       └── modules/       # Verification coverage reports
│   │           ├── index.html # Module index with statistics
│   │           └── <module>.html  # Per-module reports
│   └── ...
└── results.json          # Machine-readable results
```

## Generated Documentation

Each successful project generates a unified documentation site with:

### API Documentation (`site/api/`)
- doc-gen4 generated HTML documentation
- Searchable API reference
- Source code links

### Verification Coverage (`site/verification/`)
- **Module Index**: Overview with aggregated statistics
- **Per-Module Reports**: Fast-loading individual pages for each source file
- **Sorry Detection**: Tracks definitions and theorems containing `sorry`
- **Source Links**: Links to the actual source code on GitHub/GitLab
- **API Links**: Links to the doc-gen4 documentation for each declaration

### Statistics Tracked

| Category | Description |
|----------|-------------|
| **Definitions** | Mathematical abstractions, computational datatypes, math definitions, computational operations |
| **Theorems** | Computational, mathematical, bridging theorems |
| **Sorry Count** | Definitions and theorems with `sorry` (incomplete proofs) |

## Viewing Results

After running `./run.sh serve`:

- **Summary page**: http://localhost:9000/
- **Successful projects**: Click project name → opens `site/` directory
- **Incomplete/Failed projects**: Click "View Status" → shows command log

The summary page displays:
- Overall statistics cards (projects analyzed, definitions, theorems, bridging, sorry count)
- Sortable table of successful projects with sorry indicators (⚠️)
- Separate sections for incomplete projects (⏳) and failed projects (❌)
- Links to individual documentation sites

### Status Pages

For incomplete or failed projects, clicking "View Status" shows:
- Current state (Not Started, In Progress, Failed)
- Repository link
- Full command execution log (commands.yaml)
- Link to documentation site if partially generated

To regenerate the summary without starting a server:
```bash
./run.sh refresh
```

> **Tip:** The `refresh` command is safe to run while long-running projects (like mathlib4) are still being analyzed. Projects still running will show as "Incomplete" in the summary.

## Troubleshooting

### Build Failures

If a project fails to build:
1. Click "View Status" on the summary page to see the error
2. Check `commands.yaml` for the full command execution log
3. Common issues:
   - Mismatched Lean toolchain versions
   - Missing dependencies
   - Module name detection issues

### Module Name Detection

The script tries to detect the main module name from:
1. `lakefile.toml` → `name` field
2. `lakefile.lean` → `package` declaration

If detection fails, manually specify in `config.toml`:

```toml
[[projects]]
name = "my-project"
modules = ["ActualModuleName"]  # Override
```

### Branch Issues

If source links point to the wrong branch:
1. The branch is auto-detected from the cloned repo
2. Override with `branch = "branch-name"` in config.toml if needed
3. Re-run the project to regenerate with correct links

## Publishing to GitHub Pages

The generated `site/` folder for each project is ready for GitHub Pages deployment:

1. Copy `sites/<project>/site/` to your GitHub Pages repository
2. All links are relative, so no configuration needed
3. Works with both project pages and organization pages

## Scaling to Mathlib4

Analyzing Mathlib4 (400K+ declarations, 220K+ theorems, 7,400+ modules) required several engineering techniques to handle the scale:

### Parallel Processing

| Component | Challenge | Solution |
|-----------|-----------|----------|
| **Proof Dependencies** | Traversing proof terms for 220K theorems | Two-phase approach: Phase 1 extracts types in MetaM (sequential), Phase 2 extracts proof deps in pure IO (parallel workers) |
| **HTML Generation** | Writing 7,400+ HTML files | Parallel file writer with configurable worker count (`--html-workers N`) |

### Streaming Serialization

The classification cache (280K entries) cannot be serialized/deserialized with standard JSON due to stack overflow from recursive descent parsing. The solution uses a **split format**:

- **Metadata file** (`.json`): Small JSON with version and entry count
- **Entries file** (`.jsonl`): Pure [JSON Lines](https://jsonlines.org/) format, one entry per line

This enables:
- **Streaming writes**: One `Json.compress` call per entry, periodic flush
- **Streaming reads**: Line-by-line parsing, no recursive JSON AST construction
- **Standard tooling**: Works with `jq`, `wc -l`, `head`, `tail`, Python's `jsonlines`

### Memory and Stack Management

| Issue | Cause | Solution |
|-------|-------|----------|
| Stack overflow in RBMap traversal | Deep recursion in `NameMap.foldl` for 280K entries | Convert to Array first using tail-recursive `foldl` |
| Stack overflow in `for` loops | `for ... in` in `do` blocks accumulates continuation frames | Replace with `Array.foldl` which is guaranteed tail-recursive |
| Stack overflow in JSON serialization | Building huge `Json.arr` AST | Write entries one-at-a-time with `Json.compress` |
| UTF-8 panics in subprocess output | Invalid byte sequences in compiler warnings | Byte-safe filtering before UTF-8 conversion |
| Output buffering hiding progress | Piped I/O buffering in Lean's task scheduler | Use `.inherit` mode for direct terminal output |

**Why `Array.foldl` is stack-safe**: The Lean 4 standard library implements `Array.foldl` via `Array.foldlM` 
([Init/Data/Array/Basic.lean, line 627](https://github.com/leanprover/lean4/blob/v4.27.0-rc1/src/lean/Init/Data/Array/Basic.lean#L627)), 
which uses a tail-recursive `loop` function where the recursive call is always in tail position. 
At runtime, it's implemented by `foldlMUnsafe` 
([line 583](https://github.com/leanprover/lean4/blob/v4.27.0-rc1/src/lean/Init/Data/Array/Basic.lean#L583)) 
using `USize` indices for efficiency—also tail-recursive. This makes `Array.foldl` safe for iterating 
over 280K+ entries without stack overflow, unlike `for ... in` loops in `do` blocks which can 
accumulate continuation frames.

### Lean 4 Task Parallelism

Lean 4's runtime uses a task scheduler with important constraints on parallelism
([Init/Core.lean, lines 620-680](https://github.com/leanprover/lean4/blob/v4.27.0-rc1/src/lean/Init/Core.lean#L620)):

| Priority | Value | Behavior |
|----------|-------|----------|
| `Task.Priority.default` | 0 | Shared thread pool, limited to `LEAN_NUM_THREADS` (defaults to hardware concurrency) |
| `Task.Priority.max` | 8 | Highest priority within the shared pool |
| `Task.Priority.dedicated` | 9 | Spawns a dedicated OS thread per task |

**Why 50 vs 20 workers showed no difference**: With `Task.Priority.default`, all tasks share a pool 
capped at `LEAN_NUM_THREADS` (typically 28 on your Xeon). Spawning 50 tasks doesn't create 50 threads—
it creates 50 work items for the same ~28 threads.

**Solution for true parallelism**: Use `Task.Priority.dedicated` for compute-bound or I/O-bound workers:
```lean
-- Before: limited to thread pool size
let task ← IO.asTask (prio := .default) do ...

-- After: dedicated thread per task
let task ← IO.asTask (prio := .dedicated) do ...
```

The tradeoff: `dedicated` threads have more overhead (thread creation/destruction) but guarantee 
true parallelism. Use for long-running tasks; prefer `default` for many short tasks.

### Lean 4 Runtime Diagnostics

When debugging stack overflows or panics, the Lean 4 runtime provides several environment variables
([src/runtime/object.cpp](https://github.com/leanprover/lean4/blob/v4.27.0-rc1/src/runtime/object.cpp)):

| Variable | Purpose |
|----------|---------|
| `LEAN_BACKTRACE=1` | Print native stack trace on panic (Linux/macOS only; line 156 in object.cpp) |
| `LEAN_ABORT_ON_PANIC=1` | Abort instead of throwing exception, useful for core dumps (line 72) |
| `LEAN_NUM_THREADS=N` | Override hardware concurrency for task manager (line 1016) |

Usage for debugging stack overflow:
```bash
# Get stack trace on crash (Linux with debug symbols)
LEAN_BACKTRACE=1 LEAN_ABORT_ON_PANIC=1 .lake/build/bin/docvb unified ...

# Generate core dump for post-mortem analysis
ulimit -c unlimited
LEAN_ABORT_ON_PANIC=1 .lake/build/bin/docvb unified ...
```

For pure Lean code, use `dbgStackTrace` (Init/Util.lean line 31):
```lean
-- Print stack trace at a specific point (currently Linux only)
let _ := dbgStackTrace fun _ =>
  IO.println "About to do risky operation"
```

### docvb Overlay Architecture

Rather than forking each project, the pipeline creates a lightweight `docvb/` subdirectory that:
- Imports the parent project as a Lake dependency (via `..`)
- Adds doc-gen4 and unified-doc as dependencies
- Inherits the correct Lean toolchain from the parent
- Can be built without modifying the original project

This allows analyzing any Lean 4 project without requiring upstream changes.

### Environment Extraction

Running unified-doc with proper module resolution requires `LEAN_PATH` to be set correctly. The pipeline:
1. Runs `lake env printenv LEAN_PATH` to extract the environment
2. Passes the environment to the subprocess directly
3. Runs the binary with inherited stdout/stderr for real-time output

This approach works around a blocking I/O issue: when running `lake exe docvb ...` as a subprocess, Lake becomes an intermediary process. Output flows through a double-buffered path (`docvb` → `lake` → parent), causing progress output to appear blocked until completion. By extracting the environment and running the binary directly, we eliminate the intermediate process and get real-time streaming output.

### Potential `lake exe` Improvements

The environment extraction workaround suggests several potential improvements to Lake:

1. **`--unbuffered` or `--passthrough` flag** - Configure `lake exe` to use unbuffered I/O or inherit stdio directly when real-time output is needed.

2. **Subprocess detection** - When Lake detects it's running as a subprocess (not a TTY), it could automatically use different I/O handling to avoid double-buffering.

3. **`lake exec-env <target>` command** - A dedicated command that prints the environment needed to run an executable, making the extraction pattern first-class:
   ```bash
   lake exec-env docvb  # Outputs: LEAN_PATH=... LD_LIBRARY_PATH=... etc.
   ```

4. **Direct exec mode** - An option where Lake uses `execve` to *replace* itself with the target executable rather than spawning a child process (similar to shell's `exec` builtin).

These would benefit any tooling that needs to orchestrate Lake-built executables while preserving real-time output streaming.

## For the Paper

After running experiments, `results.json` contains all statistics in machine-readable format for generating tables and figures.

Key metrics:
- `totalDefinitions`: Sum of all definition categories
- `totalTheorems`: Sum of all theorem kinds
- `bridgingTheorems`: The key metric for spec-impl correspondence
- `defsWithSorry` / `theoremsWithSorry`: Proof completeness metrics
- Per-category breakdowns for the Four-Category Ontology analysis

