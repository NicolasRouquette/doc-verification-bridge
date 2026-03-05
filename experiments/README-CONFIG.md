# Configuration Management: Public vs Internal

This project uses two configuration files to manage public and internal deployments:

## Files

- **`config.toml`** - Local configuration (git-ignored)
  - Use this for internal/private work
  - Can include JPL-internal repositories
  - Never committed to version control

- **`config.public.toml`** - Public configuration (version controlled)
  - Use this for public GitHub deployments
  - Only includes public repositories
  - Committed and synced to public repo

## Workflow

### For Internal Development

1. Copy the public config as a starting point:
   ```bash
   cp config.public.toml config.toml
   ```

2. Add your internal projects to `config.toml`:
   ```toml
   [[projects]]
   name = "my-internal-project"
   repo = "git@github.jpl.nasa.gov:org/repo.git"
   # ...
   ```

3. Run experiments:
   ```bash
   ./run.sh  # Uses config.toml by default
   ```

### For Public Development

1. Edit `config.public.toml` to add public projects

2. Test with public config:
   ```bash
   ./run.sh config.public.toml
   ```

3. Commit changes:
   ```bash
   git add experiments/config.public.toml
   git commit -m "Add new public project to config"
   ```

### Syncing to Public GitHub

When you want to push internal changes to the public repo:

```bash
cd ~/projects/tlr/lean/doc-verification-bridge

# Fetch latest from internal origin
git fetch origin

# Merge changes (will NOT include config.toml since it's git-ignored)
git merge origin/main

# Copy the public config to the standard location
cp experiments/config.public.toml experiments/config.toml

# Commit and push to public GitHub
git add experiments/config.toml
git commit -m "Update config with public projects"
git push github main
```

## Key Benefits

✅ **Separation of concerns**: Internal projects stay internal
✅ **Easy syncing**: Most files can be merged directly
✅ **Flexibility**: Each deployment uses appropriate config
✅ **Safety**: config.toml is git-ignored, preventing accidental exposure

## Migration Notes

If you have an existing `config.toml` with internal projects:

1. Your current `config.toml` is now git-ignored (local only)
2. Edit `config.public.toml` to include public projects you want to share
3. The internal repo will track `config.public.toml` for the public version
4. The public repo will have a `config.toml` copied from `config.public.toml`
