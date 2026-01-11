# doc-verification-bridge Experiments

This directory contains the experimental pipeline for evaluating doc-verification-bridge across multiple Lean 4 projects.

## Overview

The pipeline (implemented in Lean 4):
1. Clones repositories from the configured list
2. Sets up a `docvb` subdirectory with proper lakefile.toml
3. Builds each project
4. Runs `unified-doc` with the configured classification mode (auto or annotated)
5. Generates a meta-summary page with sortable statistics

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

### Monorepo Support

For repositories where the Lean project is in a subdirectory (e.g., monorepos with multiple implementations), use the `subdirectory` field:

```toml
[[projects]]
name = "mm0"
repo = "https://github.com/digama0/mm0"
modules = ["MM0"]
subdirectory = "mm0-lean4"  # Lean 4 project is in this subdirectory
description = "Metamath Zero proof language"
```

### Classification Modes

Each project can specify its classification mode:

| Mode | Value | Description |
|------|-------|-------------|
| **Auto** | `"auto"` | Automatic heuristic-based classification (default) |
| **Annotated** | `"annotated"` | Only classify declarations with explicit `@[api_type]`, `@[api_def]`, `@[api_theorem]` annotations |

Example with annotated mode:
```toml
[[projects]]
name = "my-annotated-project"
repo = "https://github.com/user/my-annotated-project"
modules = ["MyProject"]
description = "Project using explicit annotations"
classification_mode = "annotated"
```

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

### State Tracking

Each project's state is tracked in `sites/<project>/.state`:

| State | Meaning |
|-------|--------|
| `not-started` | Project hasn't been processed yet |
| `in-progress` | Currently processing (set at start) |
| `completed` | Successfully finished |
| `failed` | Processing encountered an error |

## Output Structure

```
experiments/
├── config.toml          # Configuration
├── run.sh              # Convenience wrapper
├── repos/              # Cloned repositories
│   ├── batteries/
│   ├── mathlib4/
│   └── ...
├── sites/              # Generated documentation
│   ├── index.html      # Meta-summary page
│   ├── batteries/
│   │   ├── .state      # State file (completed/failed/in-progress)
│   │   └── site/       # MkDocs output
│   └── ...
└── results.json        # Machine-readable results
```

## Viewing Results

After running `./run.sh serve`:

- **Summary page**: http://localhost:9000/
- **Individual projects**: Click project links on the summary page (served as subdirectories, e.g., `http://localhost:9000/batteries/site/`)

The `serve` command automatically refreshes the summary page from existing coverage data before starting the HTTP server, so you always see up-to-date statistics.

To regenerate the summary without starting a server:
```bash
./run.sh refresh
```

The summary page features:
- Overall statistics cards
- Sortable table of all projects
- Links to individual documentation sites
- List of failed builds with error details

## Troubleshooting

### Build Failures

If a project fails to build:
1. Check the error page at the project's port
2. Look at the build log in `repos/<project>/`
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

## For the Paper

After running experiments, `results.json` contains all statistics in machine-readable format for generating tables and figures.

Key metrics:
- `total_definitions`: Sum of all definition categories
- `total_theorems`: Sum of all theorem kinds
- `bridging_theorems`: The key metric for spec-impl correspondence
- Per-category breakdowns for the Four-Category Ontology analysis
