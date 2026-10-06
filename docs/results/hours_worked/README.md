# Hours-worked results: collaborator-review candidate

**Not approved for external hosting. Copy only after the release conditions in
[RELEASE_STATUS.md](RELEASE_STATUS.md) are satisfied.**

This dated folder follows the previous hours-worked results layout without
overwriting it. It contains unrounded reviewer tables and selected plain source
CSVs, not respondent data, fitted objects, predictions, EIF vectors or checkpoints.

## Recorded result

The primary ATT is **-2.04 hours/week** (SE 1.15;
95% CI -4.32 to 0.24;
p=0.079). The interval includes zero. The secondary
ATE is a distinct full-population target, not a substitute headline result.

Population: Wave-I probability-sample adolescents with complete Wave-II Feelings
Scale, valid weights and the configured primary scope. This is not the Wave-I
screen-positive subset. Exposure is Wave-II CES-D >=22. Outcome is Wave-IV
fixed-cohort civilian/noninstitutional US contribution hours in qualifying jobs,
with documented contribution zeros and support 0–120 hours/week.

All 39 source CSVs record one run. Their recorded script MD5 does not match the
attached source; therefore these are recorded outputs, not a verified reproduction
of the new checkout. See [PROVENANCE.csv](PROVENANCE.csv).

## Contents

- `tables/`: primary/secondary effects, distinct policy translations, sample/design,
  response calibration, treatment overlap, baseline balance, inference, clipping,
  residual-Q comparison, MNAR and diagnostic-status summaries.
- `source_csv/`: selected aggregate source bodies, original basenames retained.
  Comment metadata is moved to documentation; only `run_id` columns are omitted.
- [MANIFEST.csv](MANIFEST.csv): every original file, its hash, purpose and disposition.
- [CSV_SCHEMA.csv](CSV_SCHEMA.csv): every source field, observed type and notes.
- [TABLE_NOTES.md](TABLE_NOTES.md): denominators, units and inference limits.

Detailed small-cell balance/disposition/status, sample-date/route counts and the
original full output inventory are deliberately withheld. Their original archive
is not included in the upload tree. The two-part MNAR reviewer table omits detailed
support counts; no scenario values or numerical primary results are changed.
