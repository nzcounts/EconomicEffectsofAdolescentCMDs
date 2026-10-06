# Economic effects of adolescent common mental disorders

R analysis of Wave-II adolescent depressive symptoms and supported adult outcomes
in restricted-use Add Health data. The configured primary target is the
survey-weighted ATT using whole-PSU cross-fitting and joint-component CV-TMLE.

This update contains v8.44 analysis definitions, not a second copy of the old
production script. It uses the existing numbered `R/` modules, `scripts/`
entry points, public configuration example and `tests/testthat/` layout.

## Collaborator starting points

- [Methods-to-code map](docs/METHODS_CODE_MAP.md)
- [Data dictionary and outcome definitions](docs/DATA_DICTIONARY.md)
- [Reproducibility and runtime requirements](docs/REPRODUCIBILITY.md)
- [Outputs and restricted-data boundary](docs/OUTPUTS.md)
- [Current diagnostic download and provenance status](docs/RESULTS_INDEX.md)
- [Review priorities](docs/REVIEW_NOTES.md)
- [Source-preservation audit](docs/REFACTOR_AUDIT.md)

## Running the analysis

Run only inside the approved restricted-use environment. Copy
`config/config.example.yml` to the ignored `config/config.yml`; resolve all
required paths and select a fresh output directory. Use the approved R/package
environment described in the reproducibility guide. Do not restore an old lock
file as though it described this run.

```bash
Rscript tests/testthat.R
Rscript scripts/run_preflight.R
Rscript scripts/run_analysis.R --config=config/config.yml
```

The production entry point calls `run_configured_analysis(cfg)`, which honours
the primary, optional repeated-split and configured-sensitivity switches.
The first two commands do not access restricted inputs. Full synthetic preflight
uses the production learner stack and is more than a lightweight syntax check.

## Scope

Default: Wave-IV unconditional current weekly hours in questionnaire-qualifying
jobs, bounded to 0–120 hours. Documented mortality/nonresident/institutional/
military/prison contribution zeros are retained; unresolved contributions remain
missing. The primary Wave-II exposure is CES-D >=22 with complete scale items.
The default is the full eligible analytic cohort, not the Wave-I depressed subset.

Other supported families: Wave-IV compensation and participation, and Wave-III/IV
education and self-rated health. Wave-III hours/earnings/participation and the
unsupported outcome families are not production routes in this source.
Optional Wave-III exposure-scope contrasts are secondary and require additional
codebook verification and distinct interpretation.

## Sharing boundary

No raw data, respondent predictions, cluster-identifiable diagnostics, fitted
objects, checkpoints, local configuration or logs belong in Git history.
Aggregate diagnostic candidates are supplied separately, with an approval gate.
Keep this repository private for review; private GitHub is still external hosting.
The producing diagnostic script has not been matched byte-for-byte to the
attached source. No diagnostic is relabelled as proof that this checkout produced it.
