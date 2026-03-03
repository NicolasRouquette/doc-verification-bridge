# Module Table Data Export

The doc-verification-bridge now exports structured table data in JSON format, providing programmatic access to the information displayed in module HTML pages.

## Output Files

When you run `unified-doc`, the following JSON files are generated:

1. **`table-data.json`**: Combined file with all module table data
2. **`table-data/`**: Directory with individual JSON files per module (e.g., `Lean4Yaml_Grammar.json`)

## Data Structures

### Definitions Table Entry

Each definition includes:
- `name`: Definition name (fully qualified)
- `category`: Classification string (e.g., "mathematicalDefinition", "computationalOperation", "mathematicalAbstraction", "computationalDatatype")
- `verifiedBy`: Array of theorems that verify this definition
  - Each theorem reference includes: `name`, `kind`, `bridgingDirection`
- `hasSorry`: Boolean indicating if the definition contains `sorry`

### Theorems Table Entry

Each theorem includes:
- `name`: Theorem name (fully qualified)
- `kind`: Classification (e.g., "computationalProperty", "mathematicalProperty", "bridgingProperty", "soundnessProperty", "completenessProperty")
- `bridgingDirection`: For bridging theorems: "sound", "complete", or "iff"
- `assumes`: Array of definition names assumed as hypotheses
- `proves`: Array of definition names this theorem proves properties about
- `validates`: Array of computational instances validated by this theorem
- `dependsOn`: Array of other theorems/lemmas used in the proof
- `hasSorry`: Boolean indicating if the proof contains `sorry`
- `axiomDeps`: Array of axiom names this proof depends on
- `proofDepTimeMs`: Time in milliseconds spent extracting proof dependencies (optional)

## Example: Reading Table Data in Python

```python
import json

# Load combined data
with open('sites/lean4-yaml-verified/table-data.json', 'r') as f:
    all_modules = json.load(f)

# Process each module
for module in all_modules:
    print(f"Module: {module['moduleName']}")

    # Definitions
    for defn in module['definitions']:
        print(f"  Definition: {defn['name']}")
        print(f"    Category: {defn['category']}")
        print(f"    Verified by {len(defn['verifiedBy'])} theorems")
        for thm in defn['verifiedBy']:
            print(f"      - {thm['name']} ({thm['kind']})")

    # Theorems
    for thm in module['theorems']:
        print(f"  Theorem: {thm['name']}")
        print(f"    Kind: {thm['kind']}")
        if thm.get('bridgingDirection'):
            print(f"    Direction: {thm['bridgingDirection']}")
        print(f"    Assumes: {len(thm['assumes'])} definitions")
        print(f"    Proves: {len(thm['proves'])} properties")
        print(f"    Validates: {len(thm['validates'])} instances")
        print(f"    Depends on: {len(thm['dependsOn'])} theorems")
        if thm.get('axiomDeps'):
            print(f"    Uses axioms: {thm['axiomDeps']}")
```

## Example: Reading in Lean

```lean
import DocVerificationBridge.TableData

open DocVerificationBridge.TableData

def main : IO Unit := do
  -- Load a specific module's table data
  let data ← ModuleTableData.loadFromFile "sites/lean4-yaml-verified/table-data/Lean4Yaml_Grammar.json"

  IO.println s!"Module: {data.moduleName}"
  IO.println s!"Definitions: {data.definitions.size}"
  IO.println s!"Theorems: {data.theorems.size}"

  -- Process definitions
  for defn in data.definitions do
    IO.println s!"  {defn.name} ({defn.category})"
    if defn.verifiedBy.size > 0 then
      IO.println s!"    Verified by: {defn.verifiedBy.map (·.name)}"

  -- Process theorems
  for thm in data.theorems do
    IO.println s!"  {thm.name}"
    if let some kind := thm.kind then
      IO.println s!"    Kind: {kind}"
    IO.println s!"    Assumes: {thm.assumes.size}, Proves: {thm.proves.size}"
```

## Use Cases

1. **Statistical Analysis**: Compute coverage metrics, theorem distribution, etc.
2. **Dependency Graphs**: Build visualization of theorem dependencies
3. **Documentation Generation**: Create custom reports or summaries
4. **CI/CD Integration**: Check verification coverage in automated pipelines
5. **Research**: Analyze verification patterns across projects

## JSON Schema

The JSON format follows standard Lean `ToJson`/`FromJson` derivations:

```typescript
interface TheoremReference {
  name: string;
  kind?: string;
  bridgingDirection?: string;
}

interface DefinitionTableEntry {
  name: string;
  category: string;
  verifiedBy: TheoremReference[];
  hasSorry: boolean;
}

interface TheoremTableEntry {
  name: string;
  kind?: string;
  bridgingDirection?: string;
  assumes: string[];
  proves: string[];
  validates: string[];
  dependsOn: string[];
  hasSorry: boolean;
  axiomDeps: string[];
  proofDepTimeMs?: number;
}

interface ModuleTableData {
  moduleName: string;
  definitions: DefinitionTableEntry[];
  theorems: TheoremTableEntry[];
}
```

## Integration

The table data export is automatically triggered during `unified-doc` execution in the `generateStaticSite` function. No additional configuration is needed.
