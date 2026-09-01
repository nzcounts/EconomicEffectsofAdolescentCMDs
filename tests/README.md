# Tests

`Rscript tests/testthat.R` runs data-free checks for module loading,
configuration overlays, public-path safety, supported outcome mappings, Wave IV
LFP and hours routing, and mortality timing.

`Rscript scripts/run_preflight.R` runs the complete synthetic estimator
preflight from `R/13_preflight.R`. It is skipped in the lightweight suite unless
`RUN_FULL_PREFLIGHT=true` because it loads the frozen learner stack.

No test reads Add Health data.
