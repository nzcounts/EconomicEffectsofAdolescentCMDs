# Restricted data boundary

No Add Health data belong in this repository. Obtain data through the applicable
Add Health and institutional process, store them only in the approved restricted
workspace, and point the ignored `config/config.yml` to those files.

Do not commit raw or derived respondent data, row-linked diagnostics, model
objects, caches, checkpoints, logs, local configuration, or small-cell outputs.
Git history is persistent: a later deletion does not make an accidental upload
safe.
