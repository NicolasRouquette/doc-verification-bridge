-- DependencyAnalysis.lean
-- Core dependency analysis engine for Lean 4 theorem classification
-- Independent of doc-gen4; can be used standalone by FGM, DVB, or other tools.

import DependencyAnalysis.Types
import DependencyAnalysis.Inference
import DependencyAnalysis.Classify
import DependencyAnalysis.Attributes
import DependencyAnalysis.Cache
import DependencyAnalysis.TableData
