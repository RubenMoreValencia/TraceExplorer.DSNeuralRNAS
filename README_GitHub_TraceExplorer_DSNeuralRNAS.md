# TraceExplorer.DSNeuralRNAS

**Observable learning exploration, visualization, traceability, and FormalSpec evidence for the DSNeuralRNAS ecosystem**

**Stable release:** `1.0.0`  
**Freeze:** `TE-1.0.0-DSNEURAL-FORMALSPEC`  
**Language:** R / Shiny  
**License:** MIT  
**Author:** Rubén Alexander More Valencia — Universidad Nacional de Piura

---

## Overview

`TraceExplorer.DSNeuralRNAS` is an independent R package and Shiny application for exploring, visualizing, comparing, and tracing the internal dynamics of learning processes produced within the DSNeuralRNAS ecosystem.

Its central idea is simple:

> **A final prediction or final loss does not fully describe learning. The learning trajectory itself is scientific evidence.**

TraceExplorer converts compatible learning objects into a canonical observable trace, preserves provenance, supports parameter and architecture inspection, integrates optional `DSNeuralRNAS.FormalSpec` semantics, and provides didactic and research-oriented visualizations for:

- loss evolution;
- gradient norm;
- learning rate;
- parameter norm and parameter velocity;
- MLP layers and parameter blocks;
- multi-run and multi-architecture comparison;
- FormalSpec windows and regimes;
- candidate regimes `Psi (Ψ)`;
- confirmed regimes `Gamma (Γ)`;
- frontier events `Phi (Φ)`;
- analytical-reference contrasts when they are explicitly available;
- evidence provenance and reproducible export.

TraceExplorer **does not retrain models, redefine FormalSpec, infer missing trajectories, or silently convert observed evidence into a control policy**.

---

## Scientific position in the DSNeuralRNAS ecosystem

The recommended architecture is to keep the ecosystem as **separate repositories/packages with explicit responsibilities**:

```text
DSNeuralRNAS
    │
    ├── produces learning processes, parameters, loss and trajectories
    │
    ▼
DSNeuralRNAS.FormalSpec
    │
    ├── defines normative observable dynamics
    │   Trace → Observables → Windows → Ψ → Γ → Φ
    │
    ├───────────────┐
    │               │
    ▼               ▼
ML.DSNeuralRNAS   TraceExplorer.DSNeuralRNAS
    │               │
    │               ├── exploration
    │               ├── visualization
    │               ├── comparison
    │               ├── traceability
    │               └── evidence
    │
    ▼
Meta-learning by dynamics
```

### Should `DSNeuralRNAS.FormalSpec` be included inside this repository?

**No.** The recommended strategy is:

1. publish `TraceExplorer.DSNeuralRNAS` as an independent repository;
2. publish `DSNeuralRNAS.FormalSpec` as a separate repository/package;
3. document FormalSpec here as an **optional scientific dependency**;
4. install it separately when FormalSpec analysis is required;
5. never copy FormalSpec source code into this repository.

This separation preserves:

- independent versioning;
- scientific traceability;
- clear responsibilities;
- reproducible dependency management;
- easier testing and maintenance;
- future reuse of FormalSpec by packages other than TraceExplorer.

In the current package metadata:

```text
Imports:
    shiny
    plotly
    DT

Suggests:
    DSNeuralRNAS.FormalSpec
    DSNeuralRNAS
    ML.DSNeuralRNAS
    testthat
    roxygen2
```

Therefore TraceExplorer has two practical modes:

### Core mode

Requires only the direct package dependencies and supports:

- canonical trace objects;
- deterministic didactic examples;
- 2D/3D exploration;
- parameter maps;
- architecture contracts;
- Evidence Stack;
- traceability;
- Shiny interface.

### Ecosystem mode

When `DSNeuralRNAS`, `DSNeuralRNAS.FormalSpec`, and/or `ML.DSNeuralRNAS` are installed, TraceExplorer additionally supports:

- real DSNeuralRNAS producer objects;
- real ML.DSNeuralRNAS learned-variable objects;
- FormalSpec analysis;
- `Ψ → Γ → Φ`;
- first-article E1–E5 evidence;
- multi-run FormalSpec comparison.

---

## Installation

### From a local source tarball

```r
install.packages(
  "TraceExplorer.DSNeuralRNAS_1.0.0.tar.gz",
  repos = NULL,
  type = "source"
)
```

### From GitHub

Replace `<GITHUB_ORG_OR_USER>` with the final GitHub account or organization:

```r
install.packages("remotes")

remotes::install_github(
  "<GITHUB_ORG_OR_USER>/TraceExplorer.DSNeuralRNAS"
)
```

### Optional ecosystem packages

If these packages are published as separate repositories:

```r
remotes::install_github(
  "<GITHUB_ORG_OR_USER>/DSNeuralRNAS"
)

remotes::install_github(
  "<GITHUB_ORG_OR_USER>/DSNeuralRNAS.FormalSpec"
)

remotes::install_github(
  "<GITHUB_ORG_OR_USER>/ML.DSNeuralRNAS"
)
```

If they are not yet publicly available, install them locally from their source package directories or release tarballs.

---

## Quick start

```r
library(TraceExplorer.DSNeuralRNAS)

run_trace_explorer()
```

This launches the Shiny application.

---

## Example 1 — Minimal observable trace

A deterministic didactic trace is included so the package can be explored without an external training package.

```r
library(TraceExplorer.DSNeuralRNAS)

tr <- te_demo_mlp_trace(30)

te_validate_trace(
  te_trace_data(tr)
)

head(
  te_trace_data(tr)
)

te_traceability_index(tr)
```

The canonical trace can include:

```text
iter
loss
grad_norm
eta
param_norm
param_velocity
delta_loss
```

plus provenance metadata.

---

## Example 2 — Learning-space exploration

```r
tr <- te_demo_mlp_trace(30)

learning_space <- te_learning_space(tr)

head(learning_space)
```

The default learning-dynamics representation uses normalized time and observable signals to make the trajectory comparable across runs.

Normalized time is:

\[
\tau_k =
\frac{k-k_{\min}}
     {k_{\max}-k_{\min}}
\]

`tau` is used for **visual alignment only**. It does not replace the original iteration and no interpolation is performed.

---

## Example 3 — FormalSpec analysis

This example requires `DSNeuralRNAS.FormalSpec`.

```r
library(TraceExplorer.DSNeuralRNAS)

tr <- te_demo_mlp_trace(30)

te_formalspec_preflight()

fs <- te_run_formalspec(tr)

te_validate_formalspec_analysis(fs)

te_formalspec_summary(fs)
```

The FormalSpec path is:

```text
Trace
  ↓
Observables
  ↓
Windows
  ↓
Ψ  candidate regime
  ↓
Γ  confirmed regime
  ↓
Φ  frontier / transition event
```

TraceExplorer does **not** silently recalibrate the frozen FormalSpec protocol.

---

## Example 4 — Evidence Stack

TraceExplorer formalizes five levels of evidence:

```text
D  Observed data
R  Final result
T  Learning trace
F  FormalSpec evidence
A  Analytical reference
```

Build an Evidence Stack:

```r
tr <- te_demo_mlp_trace(30)

stack <- te_evidence_stack(tr)

te_evidence_stack_levels(stack)

te_evidence_stack_decisions(stack)
```

Not every case must contain all five levels.

For example:

- a static validation object may not contain a learning trace;
- a real application may contain `D-R-T-F` but no analytical reference;
- a controlled analytical experiment may contain all `D-R-T-F-A`.

TraceExplorer reports unavailable evidence rather than inventing it.

---

## Example 5 — ML.DSNeuralRNAS bridge

This example requires `ML.DSNeuralRNAS`.

```r
ml_object <- te_example_ml_variable()

tr_ml <- te_bridge_ml_variable(
  ml_object
)

te_validate_trace(
  te_trace_data(tr_ml)
)

parameter_map <- te_parameter_map_ml_variable(
  ml_object
)

head(
  te_parameter_map_data(parameter_map)
)
```

For `ml_variable_aprendida`, TraceExplorer can preserve:

- the internal learning trajectory;
- parameter history;
- architecture information;
- exact parameter-map decomposition when the source provides `theta_hist`.

This makes the package useful for **meta-learning by dynamics**, where the object of study is not only the final prediction but also the trajectory through which the model learned.

---

## Example 6 — MLP architecture

```r
arch <- te_mlp_architecture(
  layer_sizes = c(2, 5, 3, 1)
)

te_validate_architecture(arch)

te_architecture_data(arch)
```

TraceExplorer separates:

1. **global dynamics**;
2. **layer dynamics**;
3. **parameter-block dynamics**;
4. **individual parameter values** through a long-format parameter map.

This design is intended to remain useful as architectures grow.

---

## Example 7 — Multi-run comparison

```r
mr <- te_example_multirun_ml(
  hidden_sizes = c(4, 5, 7),
  seeds = c(101, 202, 303)
)

te_validate_multirun(mr)

te_multirun_index(mr)

head(
  te_multirun_trace_data(mr)
)
```

Each run keeps its own identity and provenance.

No pooled or synthetic trajectory is created.

---

## Example 8 — Multi-run FormalSpec

Requires `DSNeuralRNAS.FormalSpec`.

```r
mr_fs <- te_multirun_formalspec(mr)

te_validate_multirun_formalspec(mr_fs)

te_multirun_formalspec_summary(mr_fs)

head(
  te_multirun_semantic_data(mr_fs)
)
```

FormalSpec is applied independently to each run.

---

## Example 9 — First-article E1–E5 project navigator

If the reproducible first-article project is available locally:

```r
article_dir <- "path/to/DSNeuralRNAS_FormalSpec_Article"

catalog <- te_article_catalog(
  article_dir
)

te_article_catalog_summary(
  catalog
)
```

Load an exact scientific case:

```r
case <- te_article_load_catalog_case(
  catalog,
  case_id = "E1B_050",
  experiment = "E1"
)
```

TraceExplorer distinguishes evidence roles:

```text
dynamic_pipeline
source_object
summary_or_validation
```

A summary or scope-control object is **never converted into a pseudo-trace**.

---

## Example 10 — Scientific comparison between article cases

```r
rows <- which(
  catalog$case_id %in% c(
    "E1B_050",
    "E1B_070"
  ) &
  catalog$role == "dynamic_pipeline"
)

comparison <- te_article_load_case_set(
  catalog,
  rows = rows,
  comparison_id = "E1B-comparison"
)
```

Loss views:

```r
absolute <- te_article_case_set_loss_view(
  comparison,
  scale = "absolute"
)

relative <- te_article_case_set_loss_view(
  comparison,
  scale = "relative"
)

log_relative <- te_article_case_set_loss_view(
  comparison,
  scale = "log_relative"
)
```

Formal semantic views:

```r
te_article_case_set_semantic_timeline(
  comparison
)

te_article_case_set_transition_timeline(
  comparison
)

te_article_case_set_regime_occupancy(
  comparison
)

te_article_case_set_transition_signature(
  comparison
)
```

---

## Formal analytical reference

For controlled E1 quadratic experiments, TraceExplorer can represent the known stability reference:

\[
L(\theta)
=
\frac{1}{2}\theta^\top A\theta
\]

with:

\[
\theta_{k+1}
=
(I-\eta A)\theta_k
\]

and:

\[
\eta^*
=
\frac{2}
     {\lambda_{\max}(A)}
\]

For the reference case:

```text
A = diag(1, 4)
lambda_max(A) = 4
eta* = 0.5
```

The analytical reference is enabled only when it is explicitly applicable.

It is not transferred automatically to unrelated real applications.

---

## Canonical contracts

### Observable Trace Contract

Required:

```text
iter
loss
```

Recommended when available:

```text
grad_norm
eta
param_norm
param_velocity
```

Derived:

```text
delta_loss
```

Optional:

```text
time
```

Provenance may include:

```text
experiment_id
case_id
source_package
source_class
adapter_mode
dataset_id
model_id
```

### Parameter Map Contract

Long-format structure:

```text
iter
layer
parameter_type
parameter_id
value
```

### FormalSpec Semantic Contract

```text
Trace
→ Observables
→ Windows
→ Ψ
→ Γ
→ Φ
```

### Provenance Contract

TraceExplorer preserves the origin of evidence and, for article objects, may additionally record:

- source path;
- MD5 checksum;
- experiment;
- stage;
- case;
- evidence role;
- whether original article semantics are present.

---

## Supported source classes

The default bridge registry includes support for:

```text
data.frame
dsfs_trace
rnas_neuron_train
rnas_control_eta_train
rnas_mlp_train
ml_variable_aprendida
ml_componentes_dinamicos
```

Inspect the active registry:

```r
te_bridge_registry()
```

Detect a source:

```r
te_detect_source(object)
```

Bridge it:

```r
tr <- te_bridge(object)
```

---

## Traceability

Traceability is a first-class property of the package.

```r
idx <- te_traceability_index(tr)

head(idx)
```

Depending on the available evidence, the index can connect:

```text
original iteration
observable state
source object
adapter
FormalSpec window
candidate regime Ψ
confirmed regime Γ
frontier event Φ
```

TraceExplorer follows the rule:

> **No interpretation should become detached from the evidence that produced it.**

---

## Shiny workspaces

The application currently provides workspaces for:

- home and evidence state;
- 2D trace;
- 3D exploration;
- mathematics;
- `Ψ · Γ · Φ`;
- architecture;
- MLP evidence;
- FormalSpec 3D;
- Evidence Stack;
- first-article E1–E5 navigator;
- E1–E5 scientific comparison;
- multi-run comparison;
- traceability;
- protocol;
- bridge registry.

Run:

```r
run_trace_explorer()
```

### WebGL fallback

Interactive 3D Plotly views require WebGL.

If the browser cannot provide WebGL, TraceExplorer can present a 2D fallback projection without changing the underlying evidence or FormalSpec semantics.

---

## Didactic mode and research mode

TraceExplorer is designed for two complementary uses.

### Didactic mode

The intended sequence is:

```text
data
→ update
→ trace
→ observable
→ window
→ Ψ
→ Γ
→ Φ
→ provenance
```

The goal is to explain not only *what* the model produced, but *how* the learning process evolved.

### Research mode

Research-oriented use emphasizes:

- reproducible source selection;
- case comparison;
- architecture metadata;
- exact iteration mapping;
- FormalSpec summaries;
- evidence bundles;
- source checksums;
- analytical-reference contrasts;
- multi-run behavior.

---

## Scientific guardrails

TraceExplorer follows these rules:

1. **Observe before interpreting.**
2. **Do not reconstruct missing trajectories from final metrics.**
3. **Do not invent unavailable signals.**
4. **Do not silently promote legacy semantics to FormalSpec semantics.**
5. **Reuse existing article `Ψ/Γ/Φ` when they are already available.**
6. **Run new FormalSpec analysis only explicitly.**
7. **Do not convert frontier detection directly into control action.**
8. **Do not treat normalized time `tau` as original experimental time.**
9. **Do not rank scientific cases from descriptive comparison alone.**
10. **Keep future policy/control layers separate from evidence observation.**

---

## Validation and release status

The `1.0.0` release was frozen as:

```text
TE-1.0.0-DSNEURAL-FORMALSPEC
```

Final validation:

```text
testthat:
FAIL 0
ERROR 0
WARN 0
SKIP 0
PASS 315

R CMD check RC:
0 errors
0 warnings
0 notes

Clean install + smoke RC:
PASS

R CMD check 1.0.0:
0 errors
0 warnings
0 notes

Clean install + smoke 1.0.0:
PASS
```

The release check was executed with:

```text
R CMD check --as-cran --no-manual
```

to separate package correctness from local PDF-manual toolchain dependencies.

---

## Relationship with DSNeuralRNAS.FormalSpec

TraceExplorer depends conceptually on FormalSpec for normative semantic analysis, but it should **not vendor or duplicate FormalSpec source code**.

Recommended public-repository strategy:

```text
<GitHub organization>
├── DSNeuralRNAS
├── DSNeuralRNAS.FormalSpec
├── ML.DSNeuralRNAS
└── TraceExplorer.DSNeuralRNAS
```

This makes the dependency chain visible while preserving independent releases.

For reproducible scientific work, record both freezes when applicable:

```text
TraceExplorer:
TE-1.0.0-DSNEURAL-FORMALSPEC

FormalSpec first-article baseline:
DSFS-1.0.0-FIRST-ARTICLE
```

---

## Extending TraceExplorer

New integrations should prefer extension through contracts rather than modification of the core.

Current extension points include:

```r
te_evidence_extension_points()
```

Conceptually:

```text
observed_data_adapter
final_result_adapter
trace_bridge
semantic_provider
analytical_reference_provider
decision_policy_provider
```

The final item is intentionally reserved.

A future decision-policy layer should remain separate from the evidence observer.

---

## Roadmap after 1.0.0

Potential directions include:

- additional DSNeuralRNAS bridges;
- larger neural architectures;
- richer architecture-aware observability;
- visual-learning trajectories;
- semantic-learning trajectories;
- video frame/object/track/event evidence;
- experiment-atlas reporting;
- extended analytical-reference providers;
- meta-learning observatories for `ML.DSNeuralRNAS`;
- optional policy/decision packages built **above**, not inside, TraceExplorer.

---

## Repository recommendations

For a public GitHub repository, the recommended companion files are:

```text
README.md
LICENSE
DESCRIPTION
NAMESPACE
R/
man/
tests/
inst/
NEWS.md
CITATION.cff
CONTRIBUTING.md
CODE_OF_CONDUCT.md
.github/workflows/R-CMD-check.yaml
```

Long examples should live outside the README, for example in:

```text
examples/
vignettes/
```

The README should keep only short, executable examples that explain the main workflow.

---

## Citation

Suggested software citation:

> More Valencia, R. A. (2026). **TraceExplorer.DSNeuralRNAS: Observable learning exploration, visualization, traceability, and FormalSpec evidence for the DSNeuralRNAS ecosystem** (Version 1.0.0). Universidad Nacional de Piura.

When a DOI or archival release is assigned, replace this citation with the corresponding persistent identifier.

---

## Related DSNeuralRNAS works

The package is conceptually related to the following works of the DSNeuralRNAS ecosystem:

- *DS Neural RNAS: Arquitectura de aprendizaje dinámico entre redes neuronales y dinámica de sistemas*.
- *Aplicabilidad y escalabilidad del MLP en el contexto de DSNeuralRNAS*.
- *ML.DSNeuralRNAS: Meta-Aprendizaje por Dinámica*.
- *DSNeuralRNAS.FormalSpec*.
- *ADTR.DSNeuralRNAS*.
- *Imágenes y aprendizaje visual con DSNeuralRNAS*.
- *Ecosistema Semántico DSNeuralRNAS*.

---

## License

MIT License.

See [`LICENSE`](LICENSE).

---

## Author

**Rubén Alexander More Valencia**  
Universidad Nacional de Piura  
Departamento Académico de Investigación de Operaciones  
Piura, Perú

---

## Project principle

> **TraceExplorer observes, explains, compares, and traces learning evidence.  
> It does not replace the engine that learns, the formal specification that defines semantics, or a future policy layer that decides how to act.**
