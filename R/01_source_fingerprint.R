# ADD HEALTH CAUSAL INFERENCE PIPELINE
# Estimate survey-weighted effects of Wave-II depressive-symptom exposure on
# supported adult outcomes using cluster-cross-fitted joint-component CV-TMLE.
# The headline estimand is ATT; ATE and other comparators remain labelled.
# Each outer training fold learns covariate screens, preprocessing and nuisance
# fits without using outer-validation outcomes. Whole-PSU folds preserve schools.
# Sequential outcome observation distinguishes documented contribution zeros
# from the remaining cohort and its interview-response process.
# Causal interpretation requires exchangeability, positivity, consistency and
# the stated observation-model assumptions; cross-fitting cannot establish them.
# Configuration selects the outcome, analysis scope, calibration and optional
# full-refit sensitivities or repeated-split aggregation. Synthetic preflight
# tests must pass in the configured production environment before fitting.
# Restricted inputs, respondent-level predictions, model objects and checkpoints
# remain inside the approved environment. Only disclosure-approved aggregates
# may be shared externally.
# Source the definitions, resolve configuration, then call
# run_configured_analysis(cfg). Sourcing does not run production by default.


# Resolve and hash the actual pipeline source file. The final production run
# must never rely on the version string alone for cache/checkpoint identity.
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

