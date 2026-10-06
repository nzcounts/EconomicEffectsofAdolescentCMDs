# Reproducibility

## Read before running

The code declares R 4.4.1 and exact package requirements. The diagnostic run
records R 4.4.1, but dplyr 1.2.1 rather than the code's required 1.1.4.
See [RUNTIME_VERSIONS.csv](environment/RUNTIME_VERSIONS.csv). Do not silently
disable version validation or change the recorded environment to make them agree.
Confirm the actual producing script/configuration with the analyst.

An old root `renv.lock` should be moved to `docs/archive/renv-v833.lock` when
applying this update, not treated as the v8.44 run lock. The provided historical
copy is labelled as such. A complete current lock has not been fabricated from
CSV headers: transitive dependencies and package sources cannot be recovered
from those headers alone. Ask the analyst to export `renv::snapshot()` from the
actual approved environment after resolving the source/environment discrepancy.

## Configuration

Copy `config/config.example.yml` to `config/config.yml` and edit only inside the
approved workspace. Paths are resolved relative to the YAML file. Use a new output
directory for each primary/sensitivity run; do not reuse an old directory or cache
merely because the family name is unchanged. Never commit the completed YAML.

Defaults and final source-time overrides are loaded before the local YAML overlay.
The wrapper sets source provenance to the full numbered `R/` tree. Do not source
one module in isolation. Run `scripts/run_analysis.R` from the project root.

For Wave-III exposure-scope comparisons, configure Wave-III input paths and
confirm codebook fields. `durable_control` is a post-exposure-selected secondary
contrast; disable prevalence-based translation as required by the validator.

## Validation

Lightweight CI parses/sources modules and runs structural/focused tests without
restricted inputs. The full native synthetic preflight uses the declared learner
stack; run it before production. Preparation checks here did not execute R, so
successful native tests or empirical reproduction are not claimed.

```bash
Rscript tests/testthat.R
Rscript scripts/run_preflight.R
Rscript scripts/run_analysis.R --config=config/config.yml
```

The configured production entry point also honours the enabled preflight switch.
An explicit preflight command is a separate validation session, not a reason to
bypass an enabled production check. Optional full-refit sensitivities and repeated
splits are disabled by default and may be expensive.

## Source identity

Comment cleaning changes raw file hashes even when executable bytes match.
Modularization also changes file identity. Source lineage documents the attached
file used for preparation; it does not resolve the older recorded run fingerprint.
Do not rewrite empirical CSV hashes to the new modular source hash.
