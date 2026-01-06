-- DocVerificationBridge.lean
-- Main entry point for the doc-verification-bridge library

import DocVerificationBridge.Types
import DocVerificationBridge.Inference
import DocVerificationBridge.Classify
import DocVerificationBridge.Report
import DocVerificationBridge.Attributes-- Note: Unified is imported separately in UnifiedMain.lean to avoid
-- circular dependencies with doc-gen4
