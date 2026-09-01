# Economic effects of adolescent common mental disorders

![R](https://img.shields.io/badge/R-4.4.1-blue.svg)
![Data](https://img.shields.io/badge/data-restricted--use-red.svg)

Reproducible R pipeline for estimating survey-weighted effects among adolescents
who screened above the Wave II depressive-symptom threshold. Each run selects one
verified outcome and wave, then applies the same cluster-cross-fitted CV-TMLE,
survey inference, diagnostics, and sensitivity framework.

> **Restricted-data boundary:** This repository contains no Add Health data and
> must never contain respondent-level inputs, caches, checkpoints, fitted-model
> bundles, logs, or outputs that have not passed disclosure review.

## Current production scope

The default configuration selects **Wave IV unconditional current weekly hours
worked**. The code also contains verified mappings for Wave IV labor-force
participation and compensation, and Wave III/IV educational attainment and
self-rated health. One outcome is run at a time.

| Outcome family | Supported wave(s) | Definition in the code |
|---|---:|---|
| `HoursWorked` | IV | Weekly current hours; established nonworkers receive 0; one-job workers use `H4LM19`; multiple- or unknown-job workers use `H4LM13`; capped at 120. |
| `LaborForceParticipation` | IV | Employed, temporarily absent, or unemployed and looking, using `H4LM6`, `H4LM11`, and `H4LM14`. |
| `Compensation` | IV | Exact `H4EC2` or configured `H4EC3` bracket midpoint, on the selected transform and bound. |
| `EducationalAttainment` | III, IV | Nested threshold outcomes from the verified wave-specific education fields. |
| `HealthStatus` | III, IV | At least good self-rated health. |

`UsualHours`, `MentalHealth`, and `SubstanceUse` are hard-blocked. `PassThrough`
is reserved for an explicitly configured negative-control outcome.

Mortality linkage is harmonized across supported Wave III and IV outcomes.
Death year and month are ordered against the respondent's interview month; a
same-interview-month death is not assumed to precede an observed interview
outcome. For respondents without an interview, the wave's latest complete
interview month supplies the fieldwork endpoint. When enabled, deaths classified
before the outcome receive an observed outcome value of zero.

## Quick start

1. Install R 4.4.1 and restore the environment:

   ```r
   install.packages("renv")
   renv::restore()
   ```

2. Copy `config/config.example.yml` to `config/config.yml` and edit the copy
   inside the approved restricted workspace. The local file is ignored by Git.

3. Run the synthetic preflight in a clean R session:

   ```bash
   Rscript scripts/run_preflight.R
   ```

4. Run the configured analysis:

   ```bash
   Rscript scripts/run_analysis.R --config=config/config.yml
   ```

Outputs remain beneath the configured `global.output_dir` in the restricted
workspace.

## Repository layout

```text
R/                      Numbered analysis modules, sourced in order
scripts/                Bootstrap, preflight, and production entry points
config/                 Public example configuration; local config is ignored
data/                   Restricted-data handling instructions only
tests/testthat/          Data-free structural and focused logic tests
docs/                    Methods, provenance, outputs, and review documentation
.github/workflows/       Lightweight data-free checks
```

## Documentation

- [Methods-to-code map](docs/METHODS_CODE_MAP.md)
- [Data dictionary](docs/DATA_DICTIONARY.md)
- [Reproducibility guide](docs/REPRODUCIBILITY.md)
- [Output and disclosure guide](docs/OUTPUTS.md)
- [Research background](docs/RESEARCH_BACKGROUND.md)
- [Supervisor review notes](docs/REVIEW_NOTES.md)
- [Preparation audit](docs/REFACTOR_AUDIT.md)

## Before public release

Keep the repository private until ownership, student authorship, contributor
roles, licence, and citation order are agreed. Verify that the Add Health data-use
agreement permits the documented details and obtain disclosure review for every
empirical result, table, figure, or diagnostic selected for release.
