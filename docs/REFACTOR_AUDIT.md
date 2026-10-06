# Source-preservation audit

The attached `addhealth_pipeline_v8_44(2).R` contains 22,292 newline-terminated
source lines. Source SHA-256: `90b499485a6b75d09ed95930aa4ffa26c33954147697aca49f25be4626b047ae`. Raw MD5: `c3430bac8050c5d7c7f2b781cbd0085c`.

The comment-cleaned monolithic audit copy has exactly the same non-comment
byte stream as the attachment: `6ca03ffc674116201114b0833f31d34d6fca2392102496a645b0695d7cc90f97` (SHA-256). No strings, executable
identifiers, configuration values, learner settings, tests or calculations were
changed by comment cleaning. Original historical comment headers were replaced
with functional explanations, including accurate calibration/aggregation notes.

The modular adaptation changes only the established repository interface:

1. Seventeen machine-specific input paths become `NA_character_`; real paths
   are supplied by the ignored local YAML. The nullable adjudicated-status path
   remains `NULL` as in the attachment.
2. `file_fingerprint()` accepts a source directory and fingerprints all numbered
   modules. The source-resolution error describes this interface.
3. Existing wrapper code loads all modules, applies YAML after final source-time
   defaults and calls the source's `run_configured_analysis()` dispatcher.

Source-time default assignments and the disabled guarded autorun are preserved
in `R/17_run_defaults.R`. Supplementary helper definitions remain in their source
order in `R/16_auxiliary.R`; no production definition or assertion was omitted.
Eighteen numbered R files include the config-I/O module.

See [LINEAGE.csv](audit/LINEAGE.csv) for every original line range and
[SOURCE_FILES.csv](audit/SOURCE_FILES.csv) for module hashes.
After reversing the two source-module adaptations, the complete executable
stream matches the comment-cleaned attachment exactly. Static delimiters,
configuration/path keys, links, CSV values and archive extraction are checked.
These are preparation checks, not an R preflight or restricted-data reproduction.
