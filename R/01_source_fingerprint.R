# =============================================================================
# ADD HEALTH CAUSAL INFERENCE PIPELINE
# =============================================================================
#
# WHAT THIS PROGRAM DOES, IN PLAIN LANGUAGE
# -----------------------------------------
# This program estimates whether screening above the Wave-II adolescent
# depressive-symptom threshold causally changes later labor-market outcomes
# (Wave-IV earnings, participation, and hours; Wave-III/IV education and health)
# in Add Health, which followed the same people from adolescence into adulthood. "Causes" is the
# hard word: in survey data, depressed and non-depressed teens also differ in
# many background ways (family, school, neighborhood, health), and those
# differences could produce an outcome difference on their own. The job of this
# program is to adjust for those background differences as carefully as possible
# and then estimate the part of the outcome difference attributable to depression
# itself.
#
# The quantity it estimates is the effect AMONG THE PEOPLE WHO WERE ACTUALLY
# DEPRESSED (statisticians call this the "ATT", the average effect on the
# treated). It uses CV-TMLE, combining models for the outcome, depression, and
# outcome observation. Its robustness conditions reduce reliance on any single
# nuisance-model specification. The program then runs
# a large battery of checks to see how much the answer depends on the choices
# made along the way.
#
# HOW IT WORKS, STEP BY STEP
# --------------------------
#   1. Load the survey data and the settings (all choices live in one place, the
#      USER CONFIGURATION section).
#   2. Build the three ingredients: WHO was depressed as a teen (the exposure),
#      their configured adult OUTCOME, and the many BACKGROUND variables
#      (confounders) that must be adjusted for.
#   3. Split people into groups for honest model-fitting (see "cross-fitting"
#      below) and screen the very large set of background variables down to a
#      workable one.
#   4. Fit flexible machine-learning models for the outcome and for depression and
#      combine them into the causal estimate (the CV-TMLE step).
#   5. Run diagnostics (is the estimate trustworthy?) and sensitivity analyses
#      (does it survive reasonable changes to the choices?).
#   6. Repeat under several random settings ("seeds") to confirm the answer is
#      stable and not an accident of one random split.
#
# A MAP OF THE SECTIONS
# ---------------------
#    0) USER CONFIGURATION ...... every setting you can change, gathered in one place.
#    1) PACKAGE LOADING ......... loads the required add-on libraries and checks the settings.
#    2) SMALL UTILITIES ......... little helper functions used throughout.
#    3) BASE DATA CONSTRUCTION .. builds the exposure, outcome, and covariates from raw survey items.
#    4) FOLD CONSTRUCTION ....... splits people into groups, respecting the school-based sampling.
#    5) ROUGH PRE-SCREEN ........ narrows the huge covariate set down to a manageable one.
#    6) FINAL W PREPROCESSING ... turns the covariates into the numeric matrix the models use.
#    7) LEARNER REGISTRY ........ defines the machine-learning algorithms that get used.
#    8) FINAL CV-TMLE ........... the core: fits the models and computes the causal estimate and its uncertainty.
#    9) DIAGNOSTICS ............. checks of estimation quality and robustness.
#   10) PIPELINE RUNNER ......... ties the steps together, with save/resume ("checkpointing").
#   11) PREFLIGHT UNIT TEST ..... a quick self-test on fake data to confirm the code runs end to end.
#   12) SENSITIVITY RUNNER ...... re-runs the analysis under deliberate changes to test robustness.
#   13) ENTRY POINT ............ scripts/run_analysis.R starts an explicit configured run.
#
# A FEW TERMS, IN PLAIN LANGUAGE
# ------------------------------
#   - Exposure / treatment: being depressed as an adolescent (yes/no).
#   - Outcome: configurable adult earnings, labor-force participation, hours,
#     educational attainment, or health. Supported outcomes use the same
#     wave-specific mortality-inclusive zero-composite rule.
#   - Confounder: a background variable that could influence both depression and
#     the configured outcome, and so must be adjusted for.
#   - Propensity: a person's estimated probability of having been depressed, given
#     their background; used to fairly reweight the comparison group.
#   - Nuisance models: the outcome model, depression (propensity) model, and
#     outcome-observation model. They are means to an end, not the result of interest.
#   - Cross-fitting: fitting the models on one part of the data and using them on a
#     different part, so the flexible models cannot "cheat" and overstate certainty.
#   - CV-TMLE / ATT: the robust method, and the quantity (the effect among the
#     depressed) that this program estimates.
#   - Sensitivity analysis: re-running under a changed assumption to see whether the
#     conclusion holds.
#   - Seed / multi-seed: a number that fixes the random choices so a run is exactly
#     reproducible; running many seeds shows the answer is stable across them.
#
# HOW TO RUN IT
# -------------
# Copy config/config.example.yml to the ignored config/config.yml and resolve
# its restricted-data paths. Run scripts/run_preflight.R in a clean R session,
# then run scripts/run_analysis.R. The runner sources every numbered R module,
# applies the local configuration, validates it, and writes results beneath the
# configured output directory.
#
# PIPELINE SCOPE
# --------------
# Each run selects one supported outcome family, member, and wave. Shared
# construction, screening, estimation, inference, diagnostics, and sensitivity
# machinery is then applied using the corresponding outcome definition and
# natural or configured support.
# =============================================================================

# Resolve and hash the actual pipeline source file so cache and checkpoint
# identity includes the source contents rather than only a version label.
# Resolution order is: an explicit cfg path, sourced-file frames, Rscript's
# --file argument, the active knitr input, and the active RStudio document.

resolve_pipeline_source_path <- function(explicit_path = NULL) {
  candidates <- character(0)

  if (!is.null(explicit_path) && length(explicit_path) == 1L &&
      !is.na(explicit_path) && nzchar(trimws(as.character(explicit_path)))) {
    candidates <- c(candidates, as.character(explicit_path))
  }

  frame_paths <- tryCatch({
    frames <- sys.frames()
    unlist(lapply(frames, function(fr) {
      p <- fr$ofile
      if (is.null(p) || length(p) != 1L || is.na(p) || !nzchar(as.character(p)))
        character(0) else as.character(p)
    }), use.names = FALSE)
  }, error = function(e) character(0))
  candidates <- c(candidates, frame_paths)

  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg)) candidates <- c(candidates, sub("^--file=", "", file_arg[1L]))

  if (requireNamespace("knitr", quietly = TRUE)) {
    knit_path <- tryCatch(knitr::current_input(dir = TRUE), error = function(e) "")
    if (length(knit_path) == 1L && !is.na(knit_path) && nzchar(knit_path))
      candidates <- c(candidates, knit_path)
  }

  if (interactive() && requireNamespace("rstudioapi", quietly = TRUE) &&
      isTRUE(tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE))) {
    rstudio_path <- tryCatch(rstudioapi::getSourceEditorContext()$path,
                             error = function(e) "")
    if (length(rstudio_path) == 1L && !is.na(rstudio_path) && nzchar(rstudio_path))
      candidates <- c(candidates, rstudio_path)
  }

  candidates <- unique(candidates[!is.na(candidates) & nzchar(trimws(candidates))])
  for (p in candidates) {
    if (file.exists(p))
      return(normalizePath(p, winslash = "/", mustWork = TRUE))
  }
  NA_character_
}

file_fingerprint <- function(path) {
  if (is.null(path) || length(path) != 1L || is.na(path) ||
      !nzchar(as.character(path)) || !file.exists(path)) {
    return(list(path = if (length(path)) as.character(path)[1L] else NA_character_,
                exists = FALSE, md5 = NA_character_))
  }

  # A directory fingerprint represents the complete modular source tree.
  if (dir.exists(path)) {
    root <- normalizePath(path, winslash = "/", mustWork = TRUE)
    files <- sort(list.files(root, pattern = "\\.[Rr]$", recursive = TRUE,
                             full.names = TRUE))
    if (!length(files))
      return(list(path = root, exists = FALSE, md5 = NA_character_))
    normalized <- normalizePath(files, winslash = "/", mustWork = TRUE)
    relative <- substring(normalized, nchar(root) + 2L)
    info <- file.info(normalized)
    manifest <- paste(relative, unname(tools::md5sum(normalized)),
                      unname(info$size), sep = "\t")
    manifest_path <- tempfile(pattern = "pipeline_source_manifest_", fileext = ".txt")
    on.exit(unlink(manifest_path, force = TRUE), add = TRUE)
    writeLines(manifest, manifest_path, useBytes = TRUE)
    return(list(
      path = root, exists = TRUE, size = sum(info$size),
      mtime = as.character(max(info$mtime)),
      md5 = unname(tools::md5sum(manifest_path)), n_files = length(files)))
  }

  info <- file.info(path)
  list(path = normalizePath(path, winslash = "/", mustWork = TRUE),
       exists = TRUE, size = unname(info$size), mtime = as.character(info$mtime),
       md5 = unname(tools::md5sum(path)))
}

pipeline_script_fingerprint <- function(cfg = NULL, strict = FALSE) {
  explicit <- NULL
  if (!is.null(cfg) && is.list(cfg) && !is.null(cfg$global))
    explicit <- cfg$global$pipeline_source_path
  path <- resolve_pipeline_source_path(explicit)
  fp <- file_fingerprint(path)
  valid <- isTRUE(fp$exists) && length(fp$md5) == 1L &&
    !is.na(fp$md5) && nzchar(fp$md5)
  if (!valid && isTRUE(strict)) {
    stop(paste0(
      "The pipeline source file could not be resolved and hashed. Set ",
      "cfg$global$pipeline_source_path to the source file or modular R directory before a production run."),
      call. = FALSE)
  }
  fp
}

.PIPELINE_SOURCE_PATH <- resolve_pipeline_source_path()
