#!/bin/bash
# Rsync script to copy generated sites to a docs/ folder for GitHub Pages
# Generated from config.toml project definitions

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SITES_DIR="${SCRIPT_DIR}/sites"
CONFIG_FILE="${SCRIPT_DIR}/config.toml"

# Default destination (can be overridden)
DEFAULT_DEST="${SCRIPT_DIR}/../../docs"

# Extract project names from config.toml
# Looks for lines like: name = "project-name"
read_projects_from_config() {
    if [[ ! -f "$CONFIG_FILE" ]]; then
        echo "Error: config.toml not found at $CONFIG_FILE" >&2
        exit 1
    fi
    grep -E '^name\s*=' "$CONFIG_FILE" | sed -E 's/^name\s*=\s*"([^"]+)"/\1/'
}

# Extract description for a project from config.toml
# Uses awk to find the [[projects]] block with matching name and extract description
get_project_description() {
    local project_name="$1"
    awk -v name="$project_name" '
        /^\[\[projects\]\]/ { in_project=1; current_name=""; current_desc="" }
        in_project && /^name\s*=/ { 
            gsub(/^name\s*=\s*"/, ""); gsub(/".*$/, ""); current_name=$0 
        }
        in_project && /^description\s*=/ { 
            gsub(/^description\s*=\s*"/, ""); gsub(/".*$/, ""); current_desc=$0 
        }
        in_project && /^\[/ && !/^\[\[projects\]\]/ { in_project=0 }
        in_project && current_name==name && current_desc!="" { print current_desc; exit }
    ' "$CONFIG_FILE"
}

# Read projects into array
mapfile -t PROJECTS < <(read_projects_from_config)

usage() {
    echo "Usage: $0 [OPTIONS] [DEST_DIR] [PROJECT...]"
    echo ""
    echo "Sync generated documentation sites to a destination folder."
    echo ""
    echo "Arguments:"
    echo "  DEST_DIR         Destination directory (default: ${DEFAULT_DEST})"
    echo "  PROJECT...       Specific projects to sync (default: all existing)"
    echo ""
    echo "Options:"
    echo "  -h, --help       Show this help message"
    echo "  -n, --dry-run    Show what would be synced without doing it"
    echo "  -v, --verbose    Verbose output"
    echo "  -l, --list       List available projects"
    echo "  --delete         Delete extraneous files from destination"
    echo "  --no-index       Don't generate root index.html"
    echo ""
    echo "Examples:"
    echo "  $0                           # Sync all to default docs/"
    echo "  $0 /path/to/docs             # Sync all to specified directory"
    echo "  $0 /path/to/docs PhysLean    # Sync only PhysLean"
    echo "  $0 -n                        # Dry run"
    echo "  $0 --delete                  # Sync and remove old projects"
}

list_projects() {
    echo "Available projects (from config.toml):"
    echo ""
    for project in "${PROJECTS[@]}"; do
        if [[ -d "${SITES_DIR}/${project}/site" ]]; then
            echo "  ✓ ${project}"
        else
            echo "  ✗ ${project} (not built)"
        fi
    done
}

generate_index() {
    local dest_dir="$1"
    local index_file="${dest_dir}/index.html"
    
    echo "📝 Generating root index.html..."
    
    cat > "$index_file" << 'HEADER'
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>doc-verification-bridge - Project Documentation</title>
    <style>
        :root {
            --bg-color: #1a1a2e;
            --card-bg: #16213e;
            --text-color: #eee;
            --accent-color: #0f9d58;
            --link-color: #4fc3f7;
            --border-color: #3a3a5c;
        }
        * { box-sizing: border-box; margin: 0; padding: 0; }
        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
            background: var(--bg-color);
            color: var(--text-color);
            line-height: 1.6;
            padding: 2rem;
        }
        .container { max-width: 1200px; margin: 0 auto; }
        h1 {
            text-align: center;
            margin-bottom: 0.5rem;
            font-size: 2.5rem;
        }
        .subtitle {
            text-align: center;
            color: #888;
            margin-bottom: 2rem;
        }
        .grid {
            display: grid;
            grid-template-columns: repeat(auto-fill, minmax(300px, 1fr));
            gap: 1.5rem;
        }
        .card {
            background: var(--card-bg);
            border: 1px solid var(--border-color);
            border-radius: 8px;
            padding: 1.5rem;
            transition: transform 0.2s, box-shadow 0.2s;
        }
        .card:hover {
            transform: translateY(-2px);
            box-shadow: 0 4px 12px rgba(0,0,0,0.3);
        }
        .card h2 {
            font-size: 1.25rem;
            margin-bottom: 0.5rem;
        }
        .card h2 a {
            color: var(--link-color);
            text-decoration: none;
        }
        .card h2 a:hover { text-decoration: underline; }
        .card p { color: #aaa; font-size: 0.9rem; }
        .links { margin-top: 1rem; display: flex; gap: 1rem; }
        .links a {
            color: var(--accent-color);
            text-decoration: none;
            font-size: 0.85rem;
        }
        .links a:hover { text-decoration: underline; }
        footer {
            text-align: center;
            margin-top: 3rem;
            color: #666;
            font-size: 0.85rem;
        }
        footer a { color: var(--link-color); }
    </style>
</head>
<body>
    <div class="container">
        <h1>🔍 doc-verification-bridge</h1>
        <p class="subtitle">Automatic theorem-to-function verification documentation</p>
        <div class="grid">
HEADER

    # Add a card for each existing project
    for project in "${PROJECTS[@]}"; do
        if [[ -d "${dest_dir}/${project}" ]]; then
            local desc
            desc=$(get_project_description "$project")
            if [[ -z "$desc" ]]; then
                desc="Lean 4 project"
            fi
            
            cat >> "$index_file" << CARD
            <div class="card">
                <h2><a href="${project}/index.html">${project}</a></h2>
                <p>${desc}</p>
                <div class="links">
                    <a href="${project}/index.html">📖 Documentation</a>
                    <a href="${project}/modules/index.html">📊 Verification</a>
                </div>
            </div>
CARD
        fi
    done

    cat >> "$index_file" << 'FOOTER'
        </div>
        <footer>
            <p>Generated by <a href="https://github.com/your-org/doc-verification-bridge">doc-verification-bridge</a></p>
            <p>A tool that automatically tells you which theorems verify which functions.</p>
        </footer>
    </div>
</body>
</html>
FOOTER

    echo "✅ Generated ${index_file}"
}

sync_project() {
    local project="$1"
    local dest_dir="$2"
    local site_dir="${SITES_DIR}/${project}/site"
    local project_dest="${dest_dir}/${project}"
    
    if [[ ! -d "$site_dir" ]]; then
        echo "⚠️  Skipping ${project}: site directory not found"
        return 1
    fi
    
    echo "📦 Syncing ${project}..."
    
    # Build rsync arguments
    local rsync_args="-a --checksum"
    
    if [[ "$VERBOSE" == "true" ]]; then
        rsync_args+=" -v"
    fi
    
    if [[ "$DRY_RUN" == "true" ]]; then
        rsync_args+=" -n"
    fi
    
    if [[ "$DELETE" == "true" ]]; then
        rsync_args+=" --delete"
    fi
    
    # Create destination if needed
    if [[ "$DRY_RUN" != "true" ]]; then
        mkdir -p "$project_dest"
    fi
    
    # Rsync the site contents
    rsync $rsync_args "${site_dir}/" "${project_dest}/"
    
    echo "✅ ${project} synced to ${project_dest}"
    return 0
}

# Parse arguments
DEST_DIR=""
PROJECTS_TO_SYNC=()
DRY_RUN=false
VERBOSE=false
DELETE=false
NO_INDEX=false

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            usage
            exit 0
            ;;
        -n|--dry-run)
            DRY_RUN=true
            shift
            ;;
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        -l|--list)
            list_projects
            exit 0
            ;;
        --delete)
            DELETE=true
            shift
            ;;
        --no-index)
            NO_INDEX=true
            shift
            ;;
        -*)
            echo "Unknown option: $1"
            usage
            exit 1
            ;;
        *)
            # First non-option argument is destination if it looks like a path
            if [[ -z "$DEST_DIR" && ("$1" == /* || "$1" == .* || "$1" == ~*) ]]; then
                DEST_DIR="$1"
            else
                PROJECTS_TO_SYNC+=("$1")
            fi
            shift
            ;;
    esac
done

# Set default destination if not specified
if [[ -z "$DEST_DIR" ]]; then
    DEST_DIR="$DEFAULT_DEST"
fi

# Resolve to absolute path
DEST_DIR="$(cd "$(dirname "$DEST_DIR")" 2>/dev/null && pwd)/$(basename "$DEST_DIR")" || DEST_DIR="$(pwd)/${DEST_DIR}"

# If no projects specified, sync all existing ones
if [[ ${#PROJECTS_TO_SYNC[@]} -eq 0 ]]; then
    for project in "${PROJECTS[@]}"; do
        if [[ -d "${SITES_DIR}/${project}/site" ]]; then
            PROJECTS_TO_SYNC+=("$project")
        fi
    done
fi

if [[ ${#PROJECTS_TO_SYNC[@]} -eq 0 ]]; then
    echo "No project sites found in ${SITES_DIR}/"
    echo "Run the doc-verification-bridge experiments first."
    exit 1
fi

echo "========================================"
echo "Sync to Docs Folder"
echo "========================================"
echo ""
echo "Source: ${SITES_DIR}"
echo "Destination: ${DEST_DIR}"
echo "Projects: ${PROJECTS_TO_SYNC[*]}"
if [[ "$DRY_RUN" == "true" ]]; then
    echo "Mode: DRY RUN (no changes will be made)"
fi
if [[ "$DELETE" == "true" ]]; then
    echo "Delete mode: ON (will remove extraneous files)"
fi
echo ""

# Create destination directory
if [[ "$DRY_RUN" != "true" ]]; then
    mkdir -p "$DEST_DIR"
fi

# Track results
SYNCED=()
SKIPPED=()

for project in "${PROJECTS_TO_SYNC[@]}"; do
    echo "----------------------------------------"
    if sync_project "$project" "$DEST_DIR"; then
        SYNCED+=("$project")
    else
        SKIPPED+=("$project")
    fi
done

# Generate index.html
if [[ "$NO_INDEX" != "true" && "$DRY_RUN" != "true" ]]; then
    echo "----------------------------------------"
    generate_index "$DEST_DIR"
fi

# Summary
echo ""
echo "========================================"
echo "Summary"
echo "========================================"
echo ""
echo "✅ Synced: ${#SYNCED[@]}"
for p in "${SYNCED[@]}"; do echo "   - $p"; done
echo ""
echo "⚠️  Skipped: ${#SKIPPED[@]}"
for p in "${SKIPPED[@]}"; do echo "   - $p"; done
echo ""
echo "Destination: ${DEST_DIR}"

if [[ "$DRY_RUN" == "true" ]]; then
    echo ""
    echo "This was a dry run. Run without -n to actually sync."
fi
