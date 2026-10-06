# Outputs and disclosure

Production outputs stay in the approved workspace under `global.output_dir`.
Respondent inputs, caches, predictions, EIF values, cluster-labelled diagnostics,
model/checkpoint objects, logs and local configuration do not belong in GitHub.

The optional collaborator candidates retain the previous `docs/results/hours_worked/`
convention, with a dated run subfolder so they cannot overwrite prior results:

- `tables/`: reviewer-oriented, unrounded aggregate CSVs.
- `source_csv/`: selected source tables with original basenames, no comment header,
  and process-associated `run_id` columns removed where present.
- `MANIFEST.csv`, `CSV_SCHEMA.csv`, `PROVENANCE.csv`: source purposes, field lists,
  omitted files and untouched recorded fingerprints.
- `TABLE_NOTES.md`, `README.md`, `RELEASE_STATUS.md`: interpretation and sharing gates.

Plain CSVs begin with a column header and can be previewed on GitHub. Numerical
source cells are not rounded or corrected. Missing values remain empty. Diagnostic
scenario strings stay as exported, including compatibility names; they are not
new primary runs. Header metadata is documented separately.

Detailed sparse balance, disposition/status-by-exposure, sample/date/route counts
and the complete original output inventory are not in the candidate upload tree.
The supplied archive remains the private original. The derived two-part MNAR table
omits detailed support counts while retaining scenario values and interpretation.

External-hosting/disclosure approval has not been supplied. A computational
PUBLICATION_READY marker is not that approval, and a private repository does not
replace it. Copy only approved diagnostic files after recording authorization.
