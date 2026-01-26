#!/bin/bash
# sandbox-lake.sh - Run lake commands in a bubblewrap sandbox
#
# Usage: ./sandbox-lake.sh <project-dir> <network-mode> <lake-args...>
#   project-dir:   Path to the Lean4 project directory
#   network-mode:  "network" for network access, "isolated" for no network
#   lake-args:     Arguments to pass to lake (e.g., "build", "update", "exe cache get")
#
# Examples:
#   ./sandbox-lake.sh /path/to/project network update        # Fetch dependencies
#   ./sandbox-lake.sh /path/to/project network exe cache get # Fetch mathlib cache
#   ./sandbox-lake.sh /path/to/project isolated build        # Compile (no network)
#
# Security properties:
#   - Sensitive directories (~/.ssh, ~/.gnupg, ~/.aws, etc.) are never accessible
#   - Network is controlled by the network-mode argument
#   - Process isolation via PID, UTS, IPC, and cgroup namespaces
#   - Only the project directory and cache are writable

set -euo pipefail

# --- Argument parsing ---
if [[ $# -lt 3 ]]; then
  echo "Usage: $0 <project-dir> <network-mode> <lake-args...>"
  echo ""
  echo "Arguments:"
  echo "  project-dir   Path to the Lean4 project"
  echo "  network-mode  'network' (allow network) or 'isolated' (block network)"
  echo "  lake-args     Arguments to pass to lake command"
  echo ""
  echo "Examples:"
  echo "  $0 /path/to/project network update"
  echo "  $0 /path/to/project isolated build"
  echo "  $0 /path/to/project network exe cache get"
  exit 1
fi

PROJECT_DIR="$(realpath "$1")"
NETWORK_MODE="$2"
shift 2
LAKE_ARGS=("$@")

# --- Compute workspace root ---
# For docvb subdirectories, we need to bind the parent project directory
# so Lake can resolve `require «project» from "../"` dependencies.
# We find the workspace root by looking for the nearest directory containing
# a .git directory or being two levels up from a "docvb" directory.
compute_workspace_root() {
  local dir="$1"
  # If this is a docvb directory, the workspace is the grandparent (repos/project)
  # But we want to bind the entire repos directory for safety
  if [[ "$(basename "$dir")" == "docvb" ]]; then
    # Bind two levels up (repos/project -> repos)
    dirname "$(dirname "$dir")"
  else
    # For non-docvb directories, just bind the project itself and its parent
    dirname "$dir"
  fi
}
WORKSPACE_ROOT="$(compute_workspace_root "$PROJECT_DIR")"

# --- Validation ---
if [[ ! -d "$PROJECT_DIR" ]]; then
  echo "Error: Project directory does not exist: $PROJECT_DIR" >&2
  exit 1
fi

if [[ ! -f "$PROJECT_DIR/lakefile.lean" && ! -f "$PROJECT_DIR/lakefile.toml" ]]; then
  echo "Error: No lakefile found in $PROJECT_DIR" >&2
  exit 1
fi

if [[ "$NETWORK_MODE" != "network" && "$NETWORK_MODE" != "isolated" ]]; then
  echo "Error: network-mode must be 'network' or 'isolated'" >&2
  exit 1
fi

# --- Check for bwrap ---
if ! command -v bwrap &> /dev/null; then
  echo "Error: bubblewrap (bwrap) is not installed" >&2
  echo "Install with: sudo apt install bubblewrap  # Debian/Ubuntu" >&2
  exit 1
fi

# --- Check for elan ---
if [[ ! -d "$HOME/.elan" ]]; then
  echo "Error: Elan not found at ~/.elan" >&2
  exit 1
fi

# --- Build bwrap options ---
BWRAP_OPTS=(
  --die-with-parent
  --new-session
  --unshare-pid
  --unshare-uts
  --unshare-ipc
  --unshare-cgroup
  
  # Minimal root filesystem (read-only)
  --ro-bind /usr /usr
  --ro-bind /bin /bin
  --ro-bind /etc/resolv.conf /etc/resolv.conf
  --ro-bind /etc/ssl /etc/ssl
  --ro-bind /etc/passwd /etc/passwd
  --ro-bind /etc/group /etc/group
  
  # Elan/Lean toolchain (read-only for security)
  # Toolchains are pre-installed by the GitHub Actions workflow before parallel builds start.
  # This prevents race conditions from concurrent toolchain installations and ensures
  # untrusted build scripts cannot modify the Lean toolchain.
  --ro-bind "$HOME/.elan" "$HOME/.elan"
  
  # Process isolation
  --proc /proc
  --dev /dev
  
  # Environment
  --setenv HOME "$HOME"
  --setenv PATH "$HOME/.elan/bin:/usr/bin:/bin"
  --setenv LANG "${LANG:-en_US.UTF-8}"
  --chdir "$PROJECT_DIR"
)

# --- SSL Certificate configuration (required for mathlib cache downloads) ---
# These fix "OpenSSL: unregistered scheme" errors with Azure blob storage
if [[ -f /etc/pki/tls/certs/ca-bundle.crt ]]; then
  # RHEL/Fedora/Amazon Linux style
  BWRAP_OPTS+=(--ro-bind /etc/pki /etc/pki)
  BWRAP_OPTS+=(--setenv SSL_CERT_FILE /etc/pki/tls/certs/ca-bundle.crt)
  BWRAP_OPTS+=(--setenv CURL_CA_BUNDLE /etc/pki/tls/certs/ca-bundle.crt)
elif [[ -f /etc/ssl/certs/ca-certificates.crt ]]; then
  # Debian/Ubuntu style
  BWRAP_OPTS+=(--setenv SSL_CERT_FILE /etc/ssl/certs/ca-certificates.crt)
  BWRAP_OPTS+=(--setenv CURL_CA_BUNDLE /etc/ssl/certs/ca-certificates.crt)
fi

# SSL_CERT_DIR - try common locations
if [[ -d /usr/local/ssl/certs ]]; then
  BWRAP_OPTS+=(--ro-bind /usr/local/ssl /usr/local/ssl)
  BWRAP_OPTS+=(--setenv SSL_CERT_DIR /usr/local/ssl/certs)
elif [[ -d /etc/ssl/certs ]]; then
  BWRAP_OPTS+=(--setenv SSL_CERT_DIR /etc/ssl/certs)
fi

# Conditionally add /lib and /lib64 if they exist (varies by distro)
[[ -d /lib ]] && BWRAP_OPTS+=(--ro-bind /lib /lib)
[[ -d /lib64 ]] && BWRAP_OPTS+=(--ro-bind /lib64 /lib64)

# Add ca-certificates if directory exists
[[ -d /etc/ca-certificates ]] && BWRAP_OPTS+=(--ro-bind /etc/ca-certificates /etc/ca-certificates)
[[ -d /etc/pki ]] && BWRAP_OPTS+=(--ro-bind /etc/pki /etc/pki)  # Fedora/RHEL

# Network mode
if [[ "$NETWORK_MODE" == "network" ]]; then
  BWRAP_OPTS+=(--share-net)
else
  BWRAP_OPTS+=(--unshare-net)
fi

# Writable directories: workspace root (includes project and parent deps) and cache
# We bind the workspace root to allow Lake to resolve relative path dependencies
# like `require «project» from "../"` in docvb lakefiles.
BWRAP_OPTS+=(
  --bind "$WORKSPACE_ROOT" "$WORKSPACE_ROOT"
  --bind "$HOME/.cache" "$HOME/.cache"
  --tmpfs /tmp
)

# --- Per-project mathlib cache directory ---
# When multiple projects run `lake exe cache get` concurrently, they all use
# ~/.cache/mathlib/curl.cfg for curl configuration. This causes race conditions:
# - Project A writes curl.cfg with its URLs
# - Project B overwrites it with different URLs
# - Downloads get corrupted or deletion fails with "no such file"
#
# Solution: Use MATHLIB_CACHE_DIR to give each project its own cache directory.
# We symlink the .ltar files from the shared cache to avoid re-downloading,
# but keep curl.cfg and other temp files isolated per-project.

# Extract project name from directory path for unique cache dir
PROJECT_NAME=$(basename "$PROJECT_DIR")
PROJECT_CACHE_DIR="$HOME/.cache/mathlib-projects/$PROJECT_NAME"
SHARED_CACHE="$HOME/.cache/mathlib"

mkdir -p "$PROJECT_CACHE_DIR"
mkdir -p "$SHARED_CACHE"

# Create symlinks from shared cache to project cache for existing .ltar files
# This is a one-time setup per project - new downloads go to project dir
# The .ltar files are content-addressed (hash-named) so safe to share
if [[ -d "$SHARED_CACHE" ]]; then
  for ltar in "$SHARED_CACHE"/*.ltar; do
    [[ -e "$ltar" ]] || continue  # Skip if no .ltar files exist
    ltar_name=$(basename "$ltar")
    # Only create symlink if target doesn't exist (avoid overwriting real files)
    if [[ ! -e "$PROJECT_CACHE_DIR/$ltar_name" ]]; then
      ln -sf "$ltar" "$PROJECT_CACHE_DIR/$ltar_name" 2>/dev/null || true
    fi
  done
fi

# Set the per-project cache directory - isolates curl.cfg and leantar binaries
BWRAP_OPTS+=(--setenv MATHLIB_CACHE_DIR "$PROJECT_CACHE_DIR")

# --- Execute ---
exec bwrap "${BWRAP_OPTS[@]}" -- lake "${LAKE_ARGS[@]}"
