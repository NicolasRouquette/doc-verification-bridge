# Doc-Verification-Bridge: Open Source Application Responses

## 1) This software accomplishes the following:

Doc-verification-bridge is a documentation tool for the open-source Lean4 theorem prover (https://github.com/leanprover/lean4) for classifying theorems and definitions according to their semantic role in refinement-based software development from mathematical specifications.

The tool accomplishes four primary objectives:

**Automatic Semantic Classification:** The software analyzes theorem types using heuristic inference to determine what each proof assumes, proves, and validates. It distinguishes between mathematical properties (proofs within the specification layer), computational properties (proofs about implementation behavior), and bridging properties (proofs connecting mathematical specifications to computational implementations via soundness and completeness).

**Four-Category Ontological Organization:** Based on E.J. Lowe's philosophical Four-Category Ontology, the software categorizes all tracked declarations into: (1) Mathematical Abstractions—abstract types and Prop-based structures analogous to Lowe's "Kinds"; (2) Computational Datatypes—concrete data structures analogous to "Objects"; (3) Mathematical Definitions—Prop-returning predicates and relations analogous to "Attributes"; and (4) Computational Operations—computable functions and predicates analogous to "Modes."

**Verification Coverage Reporting:** The software generates comprehensive documentation showing which specifications have been verified, which bridging theorems connect computations to specifications, and cross-references between definitions and the theorems that prove properties about them. This enables development teams to identify gaps in their verification coverage and track progress toward complete specification-implementation correspondence.

**Dual-Mode Operation:** The software operates in both automatic mode (requiring zero source code modifications, suitable for analyzing existing codebases) and manual annotation mode (providing precise control through annotation attributes for production documentation).

---

## 2) What problems are you trying to solve in the software:

The software addresses several critical challenges in verified software development:

**The Verification Coverage Gap:** When developing safety-critical autonomous systems, teams write numerous theorems but lack systematic tools to assess what has actually been verified. A project may have hundreds of proofs, yet it remains unclear which specifications have complete verification, which have only partial coverage, and which critical properties remain unproven. Doc-verification-bridge provides quantitative verification coverage metrics, enabling teams to make informed decisions about where to focus verification effort.

**The Specification-Implementation Correspondence Problem:** In verified software, there are typically two parallel worlds: mathematical specifications (Prop-valued predicates expressing desired properties) and computational implementations (Bool-returning functions that can actually execute). Proving that these correspond—that the implementation is sound (never accepts invalid inputs) and complete (accepts all valid inputs)—requires bridging theorems. These bridging proofs are the most critical artifacts for system assurance, yet existing documentation tools do not distinguish them from ordinary lemmas. Doc-verification-bridge identifies and highlights these bridging relationships.

**The Ontological Confusion Problem:** Without systematic categorization, theorem libraries become difficult to navigate. Is `IsAcyclic` a computational check or a mathematical predicate? Does `lookup_sound` prove something about the specification or the implementation? Developers waste time understanding proof structure rather than writing proofs. The Four-Category Ontology provides a principled organization that maps directly to the concerns of specification-implementation refinement.

**The Documentation-Verification Integration Problem:** Existing documentation tools (doc-gen4) and proof dependency trackers (blueprint) operate independently. Neither captures the semantic relationships between specifications and implementations. Doc-verification-bridge generates unified documentation where API reference pages link directly to verification coverage reports, enabling stakeholders to navigate from "what does this function do?" to "how do we know it's correct?"

---

## 3) What are the unique features of the software:

**Automatic Theorem Type Analysis:** Doc-verification-bridge performs deep analysis of theorem types to infer semantic classification without requiring source annotations. The inference engine recognizes patterns such as: predicate hypotheses mapping to `assumes` relationships; equation hypotheses indicating properties being proved; Bool-valued expressions in hypotheses or conclusions indicating bridging theorems; and directionality patterns (`comp = true → prop` for soundness, `prop → comp = true` for completeness).

**Four-Category Ontological Framework:** The software implements E.J. Lowe's philosophical ontology as a practical software engineering tool. This is the first documentation system for proof assistants that organizes content according to a principled metaphysical framework, distinguishing substantial from non-substantial entities and mathematical from computational domains.

**Bidirectional Cross-Referencing:** Unlike traditional API documentation that only links from definitions to their documentation, doc-verification-bridge generates bidirectional links: definitions link to theorems that prove properties about them, and theorems link to the definitions they assume, prove, or validate. This creates a navigable web of verification relationships.

**Bridging Theorem Detection:** The software automatically identifies theorems that connect decidable Boolean computations to undecidable Prop specifications. These bridging theorems are highlighted with directionality information (sound, complete, or iff), enabling reviewers to quickly assess the strength of specification-implementation correspondence.

**MkDocs Integration with doc-gen4:** The unified pipeline generates a single documentation site combining traditional API reference (via [doc-gen4](https://github.com/leanprover/doc-gen4)) with semantic verification coverage reports (via https://github.com/mkdocs/mkdocs). Users can navigate from API documentation to see "what theorems verify this function?" and from coverage reports to see "what is the API documentation for this definition?"

**Compile-Time Validation:** When using manual annotations, the software validates consistency at compile time, catching errors such as unresolved symbol references, missing required fields, and contradictory classifications before documentation is generated.

---

## 4) Abstract:

Doc-verification-bridge is a semantic documentation and verification coverage tool for Lean 4 that automatically classifies theorems according to their role in specification-implementation refinement. The tool addresses a critical gap in verified software development: while proof assistants guarantee that individual theorems are correct, they provide no systematic way to assess whether a specification is completely verified or to navigate the relationships between mathematical specifications and computational implementations.

The software implements a Four-Category Ontology inspired by E.J. Lowe's metaphysical framework, organizing declarations into Mathematical Abstractions, Computational Datatypes, Mathematical Definitions, and Computational Operations. This categorization directly maps to the concerns of specification-implementation correspondence: proving that computable Bool-returning functions correctly decide Prop-valued mathematical predicates.

The core contribution is automatic inference of semantic classification from theorem types. By analyzing the structure of hypotheses and conclusions, the tool identifies what each theorem assumes, proves, and validates—distinguishing bridging theorems (which connect specifications to implementations) from mathematical properties (internal to the specification layer) and computational properties (internal to the implementation layer). Bridging theorems are further classified by directionality: soundness proofs show implementations are conservative; completeness proofs show implementations are exhaustive.

Doc-verification-bridge operates in two modes: automatic mode requires no source modifications and is suitable for analyzing existing codebases; manual mode provides precise control through Lean 4 attributes for production documentation. The software generates MkDocs-compatible documentation integrated with doc-gen4 API reference, enabling unified navigation between "what does this function do?" and "how do we know it's correct?"

The tool is designed for safety-critical domains such as autonomous systems, where systematic verification coverage tracking is essential for regulatory compliance and assurance argumentation.

---

## 5) What improvements have been made over existing similar software applications:

Doc-verification-bridge builds upon and extends the existing Lean documentation tools, doc-gen4 (the standard API documentation generator: https://github.com/leanprover/doc-gen4) and blueprint (a proof dependency visualization tool: https://github.com/PatrickMassot/leanblueprint).

**Improvements over doc-gen4:**

Doc-gen4 generates comprehensive API documentation showing type signatures, docstrings, and source links, but treats all declarations uniformly without semantic classification. Doc-verification-bridge adds an ontological layer that categorizes declarations according to their role in specification-implementation refinement. Where doc-gen4 shows "here is a theorem," doc-verification-bridge shows "here is a bridging theorem proving that `isAcyclicBool` is sound with respect to `IsAcyclic`."

Doc-gen4 provides no mechanism for tracking verification coverage. Doc-verification-bridge generates quantitative metrics showing how many definitions have associated proofs, which specifications have complete vs. partial verification, and which critical properties remain unverified.

Doc-gen4's cross-references are limited to type-based links (clicking a type name navigates to its definition). Doc-verification-bridge adds semantic cross-references: each definition links to theorems that prove properties about it, creating bidirectional navigation between specifications and their proofs.

**Improvements over blueprint:**

Blueprint visualizes proof dependencies as directed graphs, showing which theorems depend on which lemmas. However, blueprint does not distinguish the semantic role of different proofs—a bridging theorem appears the same as an internal lemma. Doc-verification-bridge classifies each theorem by its ontological role, enabling users to filter and search by classification (e.g., "show all bridging theorems" or "show all computational properties").

Blueprint focuses on proof completion tracking (identifying `sorry` placeholders), which is valuable during development but less relevant for deployed libraries. Doc-verification-bridge focuses on specification coverage tracking, answering "which specifications have been verified?" rather than "which proofs are incomplete?"

Blueprint generates standalone visualizations separate from API documentation. Doc-verification-bridge integrates verification coverage directly with doc-gen4 output, creating a unified documentation site where API reference and verification status are co-located.

---

## 6) What advantages does this software have over existing software?

**Semantic Understanding vs. Syntactic Documentation:**

Existing tools (doc-gen4, blueprint) provide syntactic information: type signatures, dependency graphs, source locations. Doc-verification-bridge provides semantic understanding: what role does this theorem play in the verification story? Is this a specification or an implementation? Does this proof establish soundness or completeness? This semantic layer transforms documentation from a reference manual into a verification assurance artifact.

**Specification-Implementation Traceability:**

In safety-critical domains, regulatory frameworks (DO-178C, ISO 26262) require traceability between requirements, specifications, and implementations. Doc-verification-bridge provides this traceability automatically: mathematical specifications (Prop predicates) are linked to computational implementations (Bool functions) via bridging theorems, with directionality (soundness/completeness) explicitly tracked. This traceability is essential for assurance cases and certification evidence.

**Zero-Annotation Analysis:**

Unlike tools that require extensive source code modification, doc-verification-bridge operates in automatic mode on unmodified codebases. This enables immediate analysis of existing projects (e.g., Mathlib, Batteries) without coordination with upstream maintainers. Teams can assess verification coverage of third-party dependencies before investing in additional verification effort.

**Unified Documentation Experience:**

Rather than maintaining separate documentation systems (API reference in doc-gen4, proof graphs in blueprint, coverage reports in custom tooling), doc-verification-bridge generates a unified MkDocs site integrating all perspectives. Users navigate seamlessly from "what is the type signature of `lookup`?" to "what theorems prove `lookup` is correct?" to "what percentage of the Map API has been verified?"

**Principled Ontological Foundation:**

The Four-Category Ontology provides a philosophically grounded framework that generalizes beyond any specific project. Rather than ad-hoc classification schemes that vary by team, doc-verification-bridge establishes a universal vocabulary for specification-implementation refinement applicable across proof engineering projects.

**Actionable Coverage Metrics:**

Existing tools answer "what exists?" Doc-verification-bridge answers "what's missing?" By highlighting definitions without associated proofs, specifications without bridging theorems, and incomplete soundness/completeness pairs, the tool directs verification effort toward gaps rather than duplicating existing coverage.
