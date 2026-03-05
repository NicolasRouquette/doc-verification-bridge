# Syncing to Public GitHub - Step by Step

## Initial Setup (One-time)

### 1. Update the internal repo

```bash
cd /home/nfr/projects/tlr/lean/doc-verification-bridge.internal

# Stage the new files
git add experiments/.gitignore
git add experiments/config.public.toml
git add experiments/README-CONFIG.md
git add sync-to-github.sh
git add SYNC-INSTRUCTIONS.md

# Remove config.toml from tracking (keeps local copy)
git rm --cached experiments/config.toml

# Commit the changes
git commit -m "Setup dual-config system for public/internal deployments

- Add config.public.toml with only public projects
- Update .gitignore to ignore config.toml (local only)
- Add README-CONFIG.md documentation
- Add sync-to-github.sh helper script
- Keep config.toml local for internal projects"

# Push to internal origin
git push origin main
```

### 2. Sync to public GitHub

Option A: **Use the automated script**
```bash
cd /home/nfr/projects/tlr/lean/doc-verification-bridge.internal
./sync-to-github.sh
```

Option B: **Manual sync**
```bash
cd ~/projects/tlr/lean/doc-verification-bridge

# Fetch and merge from internal
git fetch origin
git merge origin/main

# Copy public config to standard location
cp experiments/config.public.toml experiments/config.toml

# Commit and push
git add experiments/config.toml
git commit -m "Update to dual-config system"
git push github main
```

## Future Syncs

Whenever you want to push internal changes to public GitHub:

```bash
cd /home/nfr/projects/tlr/lean/doc-verification-bridge.internal
./sync-to-github.sh
```

The script will:
1. ✅ Check for uncommitted changes
2. ✅ Fetch from internal origin
3. ✅ Show preview of changes
4. ✅ Merge changes
5. ✅ Update config.toml from config.public.toml
6. ✅ Optionally push to GitHub

## Config Management

### Adding Internal Projects

Edit `experiments/config.toml` (git-ignored):
```toml
[[projects]]
name = "my-internal-project"
repo = "git@github.jpl.nasa.gov:org/repo.git"
modules = ["MyModule"]
description = "Internal project"
docvb_version = "v4.29.0"
```

### Adding Public Projects

Edit `experiments/config.public.toml` (version controlled):
```toml
[[projects]]
name = "new-public-project"
repo = "https://github.com/org/repo"
modules = ["Module"]
description = "Public project"
docvb_version = "v4.29.0"
```

Then commit and sync as above.

## Key Points

- ✅ **config.toml** is now git-ignored (local only, for internal work)
- ✅ **config.public.toml** is version controlled (synced to public GitHub)
- ✅ Internal repo URL exposure is prevented
- ✅ Easy to maintain both versions
