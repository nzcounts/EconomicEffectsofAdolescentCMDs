# Tests

`Rscript tests/testthat.R` runs data-free structural and focused logic checks.
Lightweight CI does not certify a full production learner-stack run.
`Rscript scripts/run_preflight.R` runs the source's synthetic preflight in the
declared R/package environment, including focused auxiliary self-tests.
No restricted Add Health input is read by these validation entry points.
