# Collaborator review priorities

1. Resolve the source fingerprint: every selected CSV records script MD5
   `446a956fa179aeec63994fa6fe280c37`, while the attached source has a different MD5.
   This does not prove different analytic logic or multiple runs; the exact
   producing file is needed to determine what changed. Common LF/CRLF/BOM/trailing
   newline variants of the attached file do not recover the recorded hash.
2. Resolve the dplyr environment difference (code 1.1.4; recorded run 1.2.1).
   Obtain the actual resolved settings and current environment lock from the analyst.
3. Run native structural tests and full synthetic preflight on the modular checkout.
   R was unavailable during repository preparation.
4. Review cohort inclusion, contribution zeros and sequential observation assumptions.
   The primary analysis retains the fixed cohort rather than selecting on adult status.
5. Distinguish the primary ATT from the secondary ATE. Their magnitudes and targets differ.
   Do not report the more significant secondary estimate as the pre-existing headline.
6. Assess all exported baseline balance, eligibility and support diagnostics together.
   Good protected-item balance does not establish exchangeability or prove that every
   baseline variable is balanced. Plot-eligible summaries exclude sparse/filtered variables.
7. Fixed-fit clipping, inference, residual-Q and MNAR comparisons do not establish
   full-pipeline repeated-sampling coverage. Residual-Q screening is an outer-training
   heuristic with fixed-design inner validation.
8. Resolve external-hosting authorization before adding empirical files. Source
   comments and computational readiness checks do not certify disclosure approval.

The 25 diagnostic-status rows in the supplied file all report success. The original
run inventory lists 120 output artifacts but only 40 files were supplied. Missing
artifacts are not assumed to have failed; this is a selected download, not a full bundle.
