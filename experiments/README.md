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
- Adjust parallelism settings
- Set classification mode per project
- Override branch name if needed

All paths in `config.toml` are relative to the experiments directory.

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
| `lake_exe_cache_get` | bool | `false` | Run `lake exe cache get` (for mathlib4) |
| `disable_equations` | bool | `false` | Disable equation generation (avoids timeouts) |
| `skip_proof_deps` | bool | `false` | Skip proof dependency extraction entirely (fastest, but no `dependsOn` data) |
| `proof_dep_workers` | int | `0` | Upper bound on worker threads for parallel proof extraction (0 = sequential) |

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

For very large projects like mathlib4, proof dependency extraction can be slow. Two options help:

| Option | Effect | Trade-off |
|--------|--------|-----------|
| `skip_proof_deps = true` | Skips proof dependency extraction entirely | Fast, but no `dependsOn` data in reports |
| `proof_dep_workers = N` | Parallel proof extraction with up to N workers | Full data with better throughput |

**Example for mathlib4:**

```toml
[[projects]]
name = "mathlib4"
repo = "https://github.com/leanprover-community/mathlib4"
modules = ["Mathlib"]
lake_exe_cache_get = true     # Use cache for faster build
disable_equations = true       # Avoid timeout on complex proofs
proof_dep_workers = 8          # Parallel proof dep extraction with up to 8 workers
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
│   │   └── site/         # MkDocs output (for successful builds)
│   │       ├── index.html
│   │       ├── api/           # doc-gen4 API documentation
│   │       └── verification/  # Verification coverage reports
│   │           ├── index.md   # Module index with statistics
│   │           └── modules/   # Per-module reports
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

## For the Paper

After running experiments, `results.json` contains all statistics in machine-readable format for generating tables and figures.

Key metrics:
- `totalDefinitions`: Sum of all definition categories
- `totalTheorems`: Sum of all theorem kinds
- `bridgingTheorems`: The key metric for spec-impl correspondence
- `defsWithSorry` / `theoremsWithSorry`: Proof completeness metrics
- Per-category breakdowns for the Four-Category Ontology analysis
