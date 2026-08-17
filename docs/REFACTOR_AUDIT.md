# Refactor audit

The supplied production source contained 15,927 lines and had SHA-256
`ed3d466396a3911415688239aadc81c1fa7b4d14eea25c42df9ed9eb2b6071cb`.
All original line ranges were accounted for during preparation of the numbered
modules.

Three deliberate infrastructure changes were made:

1. Sixteen hard-coded local Add Health paths were replaced by `NA_character_`;
   real paths belong only in the ignored `config/config.yml`.
2. Source fingerprinting was adapted to hash the numbered files in `R/` in a
   stable order.
3. The final source-time autorun block was replaced by the explicit
   `scripts/run_analysis.R` entry point.

No estimator, exposure or outcome rule, preprocessing step, learner,
targeting equation, survey-inference calculation, diagnostic, sensitivity
scenario, or synthetic-preflight assertion was intentionally changed. The
full machine-readable lineage records and preparation utilities are retained
outside this lean public repository in the supervisor's review archive.

R was unavailable in the preparation workspace. Running `testthat` and the
complete synthetic preflight under the frozen R 4.4.1 environment therefore
remains a release requirement.
