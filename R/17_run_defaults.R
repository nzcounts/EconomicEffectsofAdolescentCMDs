# 13) AUTORUN BLOCK
# =============================================================================

# Wave-IV interview/disposition evidence identifies contribution zeros.
# Unknown status remains unverified and unresolved contributions remain missing.
# Sequential targeting separates documented zeros from the remaining cohort;
# missing-outcome identification still requires response-model assumptions.
cfg$diagnostics$enable_checkpoint_review <- TRUE
cfg$diagnostics$baseline_residual_variables <- c(
  "H1GH26", "H1FS7", "H1WP14", "H1GH15", "H1ED12", "PA12", "H1GI4", "H1GH49")
cfg$analysis$auto_military_prison_status <- TRUE
cfg$analysis$dsp4_mode <- "auto_verified"
cfg$stages$run_build_main_dataset_phase <- TRUE
cfg$cache$use_cached_main_dataset <- FALSE
cfg$global$resume_mode <- FALSE
cfg$stages$run_preflight_unit_test <- TRUE
cfg$global$autorun_pipeline <- FALSE

if (isTRUE(cfg$global$autorun_pipeline)) {
  results <- run_configured_analysis(cfg)
}
# After sourcing this file and checking cfg$paths, start ONE run with:
# results <- run_configured_analysis(cfg)


