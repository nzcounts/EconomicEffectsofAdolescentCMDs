# Supervisor review notes

## Review strengths

- The headline estimand is an explicitly enforced, jointly targeted ATT.
- Supported outcome mappings are verified and unsupported families/waves fail closed.
- Wave IV labor-force participation and hours worked use explicit route-aware rules.
- Mortality year/month classification is harmonized for Wave III and IV and is
  checked against interview timing or the wave fieldwork endpoint.
- Preprocessing and screening are nested inside whole-school outer folds.
- Outcome-observation modeling, survey-domain inference, provenance, atomic
  output writes, and publication gates are explicit.
- The synthetic preflight includes known-answer outcome and mortality tests in
  addition to estimation, inference, learner, join, and failure-path checks.

## Required decisions before public release

1. Confirm repository ownership, student authorship, contributor roles, and
   citation order before adding `CITATION.cff`.
2. Select a licence only after every code author agrees.
3. Confirm that the Add Health agreement permits the documented variable names
   and coding details.
4. Run all R tests and the full synthetic preflight under the frozen environment.
5. Run each intended outcome separately in the restricted workspace and verify
   its sample flow, mortality audit, support, diagnostics, and results.
6. Obtain disclosure review before publishing any empirical output.

## Issues to keep visible

- `HoursWorked` is current unconditional weekly hours, not the unsupported
  `UsualHours` alias. Manuscript terminology must match the implemented outcome.
- `LaborForceParticipation` is the configured Wave IV route-aware proxy and its
  exact category interpretation should be checked against the governing codebook.
- A mortality-composite zero has outcome-specific meaning: zero hours, no labor
  force participation, zero compensation, or failure to meet a binary threshold.
  Each interpretation should be defended before results are presented.
- The source supports several verified outcomes, but the default configuration
  is one specification, not a simultaneous multivariate analysis.
- `R/10_tmle.R` and `R/11_diagnostics.R` remain large to preserve reviewed order.
  Further decomposition should be a separate tested change.
- Configured sample counts are integrity gates, not reportable results until a
  frozen restricted-data run reproduces and verifies them.
- Public-facing causal language should remain qualified by the assumptions in
  `METHODS_CODE_MAP.md`.

## Suggested review sequence

1. Verify every estimand-defining toggle in `R/02_config.R`.
2. Trace exposure, selected outcome, mortality, and candidate construction in
   `R/05_data_construction.R`.
3. Review fold purity, screening, preprocessing, and learner failure gates.
4. Check ATT targeting and survey-domain influence-function inference.
5. Match required artifacts to diagnostics and the publication gate.
6. Execute the synthetic preflight before any restricted-data run.
7. Review full-refit sensitivities and the descriptive multiseed contract.
