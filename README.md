# Add Health adolescent mental health and adult earnings

![R](https://img.shields.io/badge/R-4.4.1-blue.svg)
![Data](https://img.shields.io/badge/data-restricted--use-red.svg)

Reproducible R pipeline for estimating the survey-weighted average treatment
effect among adolescents who screened above a Wave II depressive-symptom
threshold, using Wave IV compensation as the verified primary outcome. The
estimator uses school-clustered cross-fitting, Super Learner nuisance models,
joint ATT targeting, and complex-survey influence-function inference.

> **Restricted-data boundary:** This repository contains no Add Health data and
> must never contain respondent-level inputs, caches, checkpoints, fitted-model
> bundles, or nondisclosure-reviewed outputs. Run the analysis only inside an
> approved restricted-use environment.

## Scope

The supplied 15,927-line production script has been split into numbered R
modules without reorganising its statistical procedures. Local machine paths
were removed, provenance hashing was adapted to the modular `R/` directory,
and the former source-time autorun was replaced by explicit command-line entry
points.

The public entry point exposes the code-verified **Wave IV Compensation**
analysis. Other inherited outcome-family constructors are blocked unless they
are separately codebook-verified and deliberately enabled; they should not be
described as production-ready.

## Quick start

1. Install R 4.4.1 and restore the frozen environment:

   ```r
   install.packages("renv")
   renv::restore()
   ```

2. Create the ignored local configuration and enter the restricted-data paths:

   ```bash
   cp config/config.example.yml config/config.yml
   ```

3. Run the synthetic preflight in a clean R session:

   ```bash
   Rscript scripts/run_preflight.R
   ```

4. Run the configured analysis:

   ```bash
   Rscript scripts/run_analysis.R --config=config/config.yml
   ```

Outputs are written beneath the configured `global.output_dir`, which must
remain inside the restricted workspace.

## Implemented primary analysis

- **Population gate:** complete Wave II CES-D measurement and valid Wave I
  survey design information.
- **Exposure:** sum of `H2FS1`-`H2FS19`, reversing items 4, 8, 11, and 15;
  `Depressed = 1` at a score of 22 or above.
- **Outcome:** Wave IV compensation from exact `H4EC2` values or `H4EC3`
  bracket midpoints, retaining valid zeros and bounding the observed upper tail
  at the survey-weighted 0.995 quantile using the configured HF8 rule.
- **Mortality composite:** verified deaths in 1997-2007 are assigned observed
  zero earnings; Wave IV interview year is audit-only.
- **Estimator:** five whole-school outer folds; fold-specific preprocessing and
  screening; Super Learner models for the outcome, exposure propensity, and
  outcome observation; joint targeted ATT with survey-weighted,
  REGION-stratified, PSU-clustered influence-function inference.
- **Robustness:** balance, overlap, learner, cluster-influence, missingness,
  tail, clipping, MNAR, full-refit sensitivity, and multiseed diagnostics.

See [`docs/METHODS_CODE_MAP.md`](docs/METHODS_CODE_MAP.md) for the exact mapping
from methodological decisions to implementation.

## Repository layout

```text
R/                      Numbered analysis modules, sourced in order
scripts/                Bootstrap, synthetic preflight, and analysis runners
config/                 Public example configuration; local config is ignored
data/                   Restricted-data access and handling instructions only
tests/testthat/          Lightweight structural and safety tests
docs/                    Research and reproducibility documentation
.github/workflows/       Lightweight data-free CI checks
```

## Testing

Run the lightweight tests with:

```bash
Rscript tests/testthat.R
```

Run the full synthetic estimator preflight locally with:

```bash
Rscript scripts/run_preflight.R
```

No test reads Add Health data. The full preflight is not part of routine GitHub
Actions because it restores the complete learner stack and can be expensive.

## Documentation

- [Research background](docs/RESEARCH_BACKGROUND.md) — supplied Introduction,
  Literature Review, and references; placeholder Methods/Results excluded.
- [Methods-to-code map](docs/METHODS_CODE_MAP.md) — executable specification.
- [Data dictionary](docs/DATA_DICTIONARY.md) — variables, roles, and provenance.
- [Reproducibility guide](docs/REPRODUCIBILITY.md) — environment and run protocol.
- [Output guide](docs/OUTPUTS.md) — expected files and disclosure restrictions.
- [Supervisor review notes](docs/REVIEW_NOTES.md) — open decisions and cautions.
- [Refactor audit](docs/REFACTOR_AUDIT.md) — concise source-transformation record.

## Before public release

Keep the initial GitHub repository private. Before changing it to public,
confirm repository ownership and authorship, add a completed `CITATION.cff`,
select a licence with every code author, verify that the Add Health agreement
permits the documented variable and coding details, and complete disclosure
review of any empirical outputs.
