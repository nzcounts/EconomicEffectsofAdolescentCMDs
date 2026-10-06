# Table notes

CSV numerical values are unrounded. Empty cells retain missing/not-applicable
source values; they are not zeros. Original hashes and field names remain in
the manifest/schema. Plain CSVs have a column header first for GitHub preview.

## Main effects and translations

`table_1_main_effect.csv`: primary ATT among exposed versus secondary full-population
ATE, in hours/week. The design-based t reference has df 128. Never pool these targets
or report the secondary ATE as if it were the primary ATT.

`table_1_policy_translations.csv`: prevention gain is 100*(mu0/mu1-1), relative to
the exposed-group outcome mean mu1. Depression effect is 100*(mu1/mu0-1), relative
to the no-depression counterfactual mean mu0. Neither percentage is the national
change from a 1% reduction in depression prevalence. The prevalence elasticity is
a separate dimensionless local translation, conditional on the prevention and
transport assumptions; the opposite sign describes a relative prevalence decrease.

## Sample, calibration and balance

Sample/design fields retain broad analytic totals only. Missing/capped-date and
status small cells are not supplied. Design_n in source output is the full valid-weight
Wave-I frame, not the analytic cohort. Response calibration is scoped to the exporter
eligible-response cohort; documented status zeros are a separate observation source.
The propensity calibration table describes within-training selection, not outer
validation used to select the calibrator.

Treatment/trimmed/missingness balance remain distinct. The overview includes both
all exported variables and the explicitly flagged plot-eligible subset, without
discarding inconvenient imbalance. These exports are not a prespecified selected
confounder list. H1FS balance alone cannot establish absence of residual confounding.

## Inference and fixed-fit comparisons

The primary interval uses the survey-domain EIF. G* is an influence-concentration
summary, not the nominal number of survey degrees of freedom. PSU-only, leave-one-
cluster-out and multiplier variants hold nuisance fits fixed. They are supplemental
diagnostics, not full-pipeline refit uncertainty. Folds share training data and are
not five independent studies. Clip rows retarget the saved fit; full-refit code
sensitivities are a separate operation. Do not select a bound based on significance.
Q-comparison rows keep other nuisances fixed and are not independent complete runs.

## MNAR and additive bias

The standard MNAR grid fixes nuisance estimates, outcome support and primary SE;
its interval endpoints are conditional. mnar_points thresholds null/halve the point
estimate, not its confidence interval. Two-part MNAR perturbs only observed
unresolved-status missing patterns using added zero-mixture fractions and mean
shifts; it supplies point estimates without scenario confidence intervals.
Documented zeros remain unchanged. Its leverage differs from the fitted-response
standard grid. Additive-bias scenarios posit fixed external bias, not partial-R2
confounding strength or uncertainty in the bias itself.

## Completeness

All 25 supplied status rows report success. The original inventory lists 120
artifacts; this selected download has 40 files. Status successes do not supply
missing tables, source-code identity or disclosure approval. No unavailable table,
figure, full-refit sensitivity or bootstrap-positivity result has been invented.
