# Restricted data boundary

No Add Health data are distributed with this repository. Access to the
restricted-use inputs must be obtained directly from the National Longitudinal
Study of Adolescent to Adult Health and used only under the applicable data-use
agreement and institutional controls.

## Expected configured sources

The production configuration names 16 sources: Wave I in-home data, birth
records, neighborhood/grouping data, Wave I in-school data, three Wave I
contextual files, spatial and state-characteristic files, policy-context data,
Wave I weights, school-administrator data, Waves II-V in-home files, and the NDI
2019 mortality linkage. Exact local paths belong only in
`config/config.yml`, which is ignored by Git.

## Files that must not be committed

- Raw `.xpt`, `.sas7bdat`, `.dta`, or `.sav` files.
- Merged Wave I or analytic-sample `.rds`/`.rda` caches.
- Checkpoints, fitted Super Learner objects, or `diagnostic_fit_bundle.rds`.
- Respondent-, school-, PSU-, or cluster-linked diagnostic extracts.
- Results, plots, logs, or tables that have not passed the applicable
  disclosure review.

Only synthetic fixtures and explicitly disclosure-reviewed aggregate outputs
may be considered for version control. The current repository includes neither.

## Input validation

`scripts/run_analysis.R` checks that every source required by the selected
analysis exists before the pipeline begins. The production code then validates
join keys, canonical survey design fields, source provenance, expected sample
gates, and mortality linkage.
