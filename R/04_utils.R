# Generated from the reviewed v8.28 production source.
# Original lines: 1856-3337.
# Module role: Shared utilities and provenance helpers.
# See docs/REFACTOR_AUDIT.md for the exact transformation record.

# 2) SMALL UTILITIES
# =============================================================================

msg <- function(..., cfg) { if (isTRUE(cfg$global$verbose)) message(...) }

seed_for <- function(cfg, offset = 0L) as.integer(cfg$global$pipeline_seed + offset)

with_local_seed <- function(seed, value) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  on.exit({
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(as.integer(seed))
  force(value)
}

ensure_output_dir <- function(path_dir, cfg = NULL, test_write = FALSE) {
  if (is.null(path_dir) || length(path_dir) != 1L || is.na(path_dir) ||
      !nzchar(trimws(as.character(path_dir))))
    stop("Output directory must be one nonblank path.", call. = FALSE)
  path_dir <- as.character(path_dir)
  if (!dir.exists(path_dir)) {
    ok <- dir.create(path_dir, recursive = TRUE, showWarnings = TRUE)
    if (!isTRUE(ok) && !dir.exists(path_dir))
      stop("Could not create output directory: ", path_dir, call. = FALSE)
  }
  if (!dir.exists(path_dir))
    stop("Output directory does not exist after creation attempt: ", path_dir,
         call. = FALSE)
  if (!is.null(cfg)) assert_path_length_safe(path_dir, cfg, "output directory")
  if (isTRUE(test_write)) {
    canary <- tempfile(pattern = ".addhealth_canary_", tmpdir = path_dir,
                       fileext = ".txt")
    on.exit(if (file.exists(canary)) unlink(canary, force = TRUE), add = TRUE)
    write_ok <- tryCatch({
      writeLines("addhealth_write_test", canary, useBytes = TRUE)
      identical(readLines(canary, warn = FALSE), "addhealth_write_test")
    }, error = function(e) FALSE)
    if (!isTRUE(write_ok) || !file.exists(canary))
      stop("Output directory is not reliably writable/readable: ", path_dir,
           call. = FALSE)
    if (!isTRUE(unlink(canary, force = TRUE) == 0L) && file.exists(canary))
      stop("Output directory canary could not be removed: ", canary, call. = FALSE)
  }
  invisible(path_dir)
}

assert_path_length_safe <- function(path, cfg, label = "path") {
  if (.Platform$OS.type != "windows") return(invisible(TRUE))
  limit <- as.integer(cfg$safety$windows_max_path %||% 259L)
  if (!is.finite(limit) || limit < 100L)
    stop("safety$windows_max_path must be a sensible positive integer.", call. = FALSE)
  n <- nchar(as.character(path), type = "chars")
  if (n > limit)
    stop(sprintf("%s exceeds the configured Windows path limit (%d > %d): %s",
                 label, n, limit, path), call. = FALSE)
  invisible(TRUE)
}

atomic_commit_file <- function(temp_path, final_path, overwrite = FALSE) {
  if (!file.exists(temp_path))
    stop("Atomic write temporary file does not exist: ", temp_path, call. = FALSE)
  ensure_output_dir(dirname(final_path), test_write = FALSE)
  backup <- NULL
  if (file.exists(final_path)) {
    if (!isTRUE(overwrite))
      stop("Refusing to overwrite existing output: ", final_path, call. = FALSE)
    backup <- tempfile(pattern = paste0(".", basename(final_path), ".bak_"),
                       tmpdir = dirname(final_path), fileext = ".tmp")
    if (!file.rename(final_path, backup))
      stop("Could not move existing file aside for atomic replacement: ", final_path,
           call. = FALSE)
  }
  committed <- file.rename(temp_path, final_path)
  if (!isTRUE(committed)) {
    restored <- TRUE
    if (!is.null(backup) && file.exists(backup))
      restored <- isTRUE(file.rename(backup, final_path))
    if (!isTRUE(restored))
      stop(paste0(
        "Atomic rename failed and the previous file could not be restored. ",
        "Temporary file: ", temp_path, "; backup: ", backup,
        "; destination: ", final_path), call. = FALSE)
    stop("Atomic rename failed for: ", final_path, call. = FALSE)
  }
  if (!is.null(backup) && file.exists(backup)) {
    removed <- unlink(backup, force = TRUE)
    if (!identical(as.integer(removed), 0L) && file.exists(backup))
      warning("Atomic replacement succeeded, but its backup could not be removed: ",
              backup, call. = FALSE)
  }
  if (!file.exists(final_path))
    stop("Atomic replacement reported success but the destination is absent: ",
         final_path, call. = FALSE)
  invisible(final_path)
}

atomic_save_rds <- function(object, path, overwrite = TRUE, version = 2L,
                            verify = TRUE,
                            full_readback_max_bytes = 128 * 1024^2) {
  ensure_output_dir(dirname(path), test_write = FALSE)
  if (!is.numeric(full_readback_max_bytes) ||
      length(full_readback_max_bytes) != 1L ||
      is.na(full_readback_max_bytes) ||
      !is.finite(full_readback_max_bytes) ||
      full_readback_max_bytes < 0)
    stop("full_readback_max_bytes must be one nonnegative finite number.",
         call. = FALSE)
  tf <- tempfile(pattern = paste0(".", basename(path), "_"),
                 tmpdir = dirname(path), fileext = ".tmp")
  on.exit(if (file.exists(tf)) unlink(tf, force = TRUE), add = TRUE)
  saveRDS(object, tf, version = version)
  temp_info <- file.info(tf)
  if (!file.exists(tf) || !is.finite(temp_info$size) || temp_info$size <= 0)
    stop("RDS serialization did not produce a nonempty temporary file for: ",
         path, call. = FALSE)
  # Full read-back is valuable for ordinary outputs but can double peak memory
  # for the large Wave-I/main-data caches. Large files are therefore verified
  # by successful serialization plus nonzero/stable byte size across the atomic
  # rename; smaller files are additionally deserialized before commit.
  if (isTRUE(verify) && temp_info$size <= full_readback_max_bytes) {
    chk <- tryCatch(readRDS(tf), error = function(e) e)
    if (inherits(chk, "error"))
      stop("RDS verification failed before commit for ", path, ": ",
           conditionMessage(chk), call. = FALSE)
  }
  expected_size <- as.numeric(temp_info$size)
  atomic_commit_file(tf, path, overwrite = overwrite)
  final_info <- file.info(path)
  if (!file.exists(path) || !is.finite(final_info$size) ||
      as.numeric(final_info$size) != expected_size)
    stop("RDS byte-size verification failed after atomic commit for: ", path,
         call. = FALSE)
  invisible(path)
}

atomic_write_plain_csv <- function(x, path, row.names = FALSE, overwrite = FALSE,
                                   verify = TRUE) {
  xx <- as.data.frame(x)
  if (ncol(xx) < 1L)
    stop("Refusing to write a zero-column CSV artifact: ", path, call. = FALSE)
  ensure_output_dir(dirname(path), test_write = FALSE)
  tf <- tempfile(pattern = paste0(".", basename(path), "_"),
                 tmpdir = dirname(path), fileext = ".tmp")
  on.exit(if (file.exists(tf)) unlink(tf, force = TRUE), add = TRUE)
  utils::write.csv(xx, tf, row.names = row.names, na = "")
  if (isTRUE(verify)) {
    chk <- tryCatch(utils::read.csv(tf, stringsAsFactors = FALSE,
                                   check.names = FALSE), error = function(e) e)
    if (inherits(chk, "error") || nrow(chk) != nrow(xx) ||
        ncol(chk) != ncol(xx))
      stop("CSV verification failed before commit for: ", path, call. = FALSE)
  }
  atomic_commit_file(tf, path, overwrite = overwrite)
}

assert_fresh_output_dir <- function(path_dir, cfg, allowed_basenames = NULL) {
  if (!isTRUE(cfg$safety$require_fresh_primary_output_dir %||% TRUE) ||
      isTRUE(cfg$global$resume_mode %||% FALSE)) return(invisible(TRUE))
  ensure_output_dir(path_dir, cfg = cfg, test_write = TRUE)
  allowed <- if (is.null(allowed_basenames))
    cfg$safety$fresh_output_allowed_basenames %||% character(0) else
    allowed_basenames
  if (!is.character(allowed) || anyNA(allowed) || any(!nzchar(trimws(allowed))) ||
      any(basename(allowed) != allowed))
    stop("Fresh-output allowlist must contain nonblank base filenames only.",
         call. = FALSE)
  existing <- list.files(path_dir, all.files = TRUE, no.. = TRUE, full.names = FALSE)
  existing <- setdiff(existing, unique(allowed))
  if (length(existing))
    stop("Fresh-run safety gate: output directory is not empty: ", path_dir,
         ". Existing entries: ", paste(head(existing, 20L), collapse = ", "),
         call. = FALSE)
  invisible(TRUE)
}

assert_planned_output_paths <- function(cfg, extra_filenames = character(0)) {
  scalar_strings <- unlist(cfg, recursive = TRUE, use.names = FALSE)
  file_names <- unique(c(
    scalar_strings[grepl("\\.(csv|rds|png|txt)$", scalar_strings, ignore.case = TRUE)],
    extra_filenames))
  if (!length(file_names)) return(invisible(TRUE))
  tag <- build_run_tag(cfg)
  candidates <- unlist(lapply(file_names, function(f) {
    b <- basename(f)
    c(build_unique_path(cfg, b),
      build_unique_diag_path(cfg,
        file.path(cfg$global$output_dir, cfg$diagnostics$diagnostics_dir), b),
      file.path(cfg$global$output_dir, paste0("sens_", tag), b))
  }), use.names = FALSE)
  for (p in candidates) assert_path_length_safe(p, cfg, "planned output path")
  invisible(TRUE)
}


# v6: Build a unique, self-describing filename for an output file.
# The filename includes the base name, a tag summarizing the run (family,
# wave, family_member, run_label), and an optional timestamp so
# repeat runs never overwrite each other.
# Example: "cv_tmle_results__HealthStatus__wave3__at_least_good__2026-04-23_143205.csv"
build_run_tag <- function(cfg) {
  parts <- character(0)
  if (!is.null(cfg$outcome$family))        parts <- c(parts, cfg$outcome$family)
  if (!is.null(cfg$outcome$current_wave))  parts <- c(parts, sprintf("wave%d", cfg$outcome$current_wave))
  if (!is.null(cfg$outcome$family_member)) parts <- c(parts, cfg$outcome$family_member)
  if (!is.null(cfg$global$run_label) && nzchar(cfg$global$run_label))
    parts <- c(parts, cfg$global$run_label)
  if (length(parts) == 0L) parts <- "run"
  tag <- paste(parts, collapse = "__")
  tag <- gsub("[^A-Za-z0-9._=-]+", "_", tag)
  tag
}

ensure_run_id <- function(cfg) {
  if (is.null(cfg$global$run_id) || !nzchar(as.character(cfg$global$run_id))) {
    cfg$global$run_id <- paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), "__pid", Sys.getpid())
  }
  cfg
}

runtime_provenance <- function() {
  pkgs <- c("haven", "dplyr", "purrr", "SuperLearner", "glmnet", "survey",
            "ranger", "xgboost", "Matrix", "earth", "mgcv", "nnet", "e1071")
  installed <- pkgs[vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  versions <- if (length(installed)) {
    paste(sprintf("%s=%s", installed,
                  vapply(installed, function(pkg) as.character(utils::packageVersion(pkg)), character(1))),
          collapse = ";")
  } else ""
  list(
    R_version = paste(R.version$major, R.version$minor, sep = "."),
    platform = R.version$platform,
    packages = versions)
}

build_unique_diag_path <- function(cfg, out_dir, base_filename) {
  # Same naming scheme as build_unique_path but rooted under a custom
  # subdirectory (e.g., the diagnostics folder).
  ext_match <- regexpr("\\.[A-Za-z0-9]+$", base_filename)
  if (ext_match > 0L) {
    stem <- substr(base_filename, 1L, ext_match - 1L)
    ext  <- substr(base_filename, ext_match, nchar(base_filename))
  } else {
    stem <- base_filename; ext <- ""
  }
  tag <- build_run_tag(cfg)
  ts  <- if (isTRUE(cfg$global$append_timestamp_to_outputs))
           as.character(cfg$global$run_id %||% format(Sys.time(), "%Y-%m-%d_%H%M%S")) else NULL
  pieces <- c(stem, tag, ts); pieces <- pieces[nzchar(pieces)]
  fname  <- paste0(paste(pieces, collapse = "__"), ext)
  file.path(out_dir, fname)
}

build_unique_path <- function(cfg, base_filename) {
  ext_match <- regexpr("\\.[A-Za-z0-9]+$", base_filename)
  if (ext_match > 0L) {
    stem <- substr(base_filename, 1L, ext_match - 1L)
    ext  <- substr(base_filename, ext_match, nchar(base_filename))
  } else {
    stem <- base_filename; ext <- ""
  }
  tag <- build_run_tag(cfg)
  ts  <- if (isTRUE(cfg$global$append_timestamp_to_outputs))
           as.character(cfg$global$run_id %||% format(Sys.time(), "%Y-%m-%d_%H%M%S")) else NULL
  pieces <- c(stem, tag, ts)
  pieces <- pieces[nzchar(pieces)]
  fname  <- paste0(paste(pieces, collapse = "__"), ext)
  file.path(cfg$global$output_dir, fname)
}

configured_outcome_definition <- function(cfg) {
  mort <- cfg$mortality_sensitivity %||% list()
  mortality_txt <- if (isTRUE(mort$enabled) && isTRUE(mort$composite_zero_at_death)) {
    sprintf(
      paste0("mortality-inclusive past-year earnings composite; deaths %d-%d assigned observed zero; ",
             "death classification uses %s independently of Wave-IV interview participation; ",
             "%s is audit-only"),
      as.integer(mort$death_year_start), as.integer(mort$death_year_end),
      mort$source_var %||% "NDIDD19Y", mort$interview_year_var %||% "IYEAR4")
  } else {
    "ordinary configured outcome"
  }
  cap_q <- cfg$outcome$continuous_upper_quantile %||% NA_real_
  cap_txt <- if (is.finite(cap_q) && cap_q < 1)
    sprintf("weighted upper cap q=%.4f", cap_q) else "no quantile cap"
  price_txt <- if (identical(cfg$outcome$family, "Compensation"))
    (mort$earnings_price_basis %||% "nominal_past_year_dollars_no_inflation_adjustment") else
    "configured outcome units"
  paste(mortality_txt, cap_txt, price_txt, sep = "; ")
}


compensation_ratio_translation_enabled <- function(cfg) {
  family <- cfg$outcome$family %||% NA_character_
  fam_cfg <- if (!is.null(family) && family %in% names(cfg$outcome$families))
    cfg$outcome$families[[family]] else list()
  identical(family, "Compensation") &&
    identical(tolower(cfg$outcome$compensation_transform %||% "identity"),
              "identity") &&
    isTRUE(fam_cfg$report_ratio_translations %||% FALSE)
}

configured_outcome_scale <- function(cfg, outcome_type = NULL) {
  if (!is.null(outcome_type) && identical(outcome_type, "binary"))
    return("binary_probability")
  if (identical(cfg$outcome$family, "Compensation")) {
    tr <- tolower(cfg$outcome$compensation_transform %||% "identity")
    if (identical(tr, "identity")) return("raw_dollars")
    return(paste0("compensation_", tr))
  }
  "continuous_original_units"
}

archive_existing_path <- function(path, label = "ARCHIVED") {
  if (!file.exists(path) && !dir.exists(path)) return(invisible(NA_character_))
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  dest <- paste0(path, "__", sanitize_piece(label), "__", stamp,
                 "__pid", Sys.getpid())
  k <- 0L
  while (file.exists(dest) || dir.exists(dest)) {
    k <- k + 1L
    dest <- paste0(path, "__", sanitize_piece(label), "__", stamp,
                   "__pid", Sys.getpid(), "_", k)
  }
  if (!file.rename(path, dest))
    stop("Could not archive existing path before a fresh write: ", path,
         call. = FALSE)
  invisible(dest)
}

strip_runtime_config_state <- function(cfg, analysis_only = FALSE) {
  z <- cfg
  if (!is.null(z$preprocessing)) {
    z$preprocessing$global_missing_dictionary <- NULL
    z$preprocessing$variable_source_registry <- NULL
  }
  if (!is.null(z$final_tmle)) {
    z$final_tmle$screen_y_lower <- NULL
    z$final_tmle$screen_y_upper <- NULL
  }
  if (!is.null(z$provenance)) z$provenance <- NULL
  if (isTRUE(analysis_only)) {
    if (!is.null(z$global)) {
      runtime_global_fields <- c(
        "output_dir", "run_id", "run_label", "checkpoint_subdir",
        "pipeline_source_path", "append_timestamp_to_outputs",
        "resume_mode")
      for (nm in runtime_global_fields) z$global[[nm]] <- NULL
    }
    # Software versions and file-handling controls are frozen provenance and
    # execution safeguards, not features of the causal estimand or nuisance
    # specification. Keep them in resolved_run_config_md5, but exclude them
    # from analysis_spec_md5 so an operational resume setting cannot masquerade
    # as an analytical change.
    z$runtime <- NULL
    if (!is.null(z$safety)) {
      runtime_safety_fields <- c(
        "require_fresh_primary_output_dir", "fresh_output_allowed_basenames",
        "allow_output_overwrite", "verify_atomic_writes",
        "windows_max_path", "require_publication_ready_marker")
      for (nm in runtime_safety_fields) z$safety[[nm]] <- NULL
    }
  }
  z
}

freeze_run_provenance <- function(cfg) {
  cfg$policy$outcome_scale_label <- configured_outcome_definition(cfg)
  if (is.null(cfg$global$run_label) || !nzchar(trimws(as.character(cfg$global$run_label))))
    cfg$global$run_label <- "primary"
  source_fp <- pipeline_script_fingerprint(
    cfg, strict = isTRUE(cfg$global$require_script_md5 %||% FALSE))
  cfg$provenance <- list(
    analysis_spec_md5 = object_md5(strip_runtime_config_state(cfg, analysis_only = TRUE)),
    resolved_run_config_md5 = object_md5(strip_runtime_config_state(cfg, analysis_only = FALSE)),
    outcome_definition = configured_outcome_definition(cfg),
    source_fingerprint = source_fp,
    source_files = lapply(cfg$paths, file_fingerprint),
    runtime = runtime_provenance(),
    frozen_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
  cfg
}

get_frozen_source_fingerprint <- function(cfg) {
  fp <- cfg$provenance$source_fingerprint %||% list()
  if (!isTRUE(fp$exists) || is.null(fp$md5) || is.na(fp$md5) || !nzchar(fp$md5))
    stop("Run provenance does not contain a valid frozen source fingerprint.",
         call. = FALSE)
  fp
}

verify_frozen_source_unchanged <- function(cfg) {
  frozen <- get_frozen_source_fingerprint(cfg)
  current <- file_fingerprint(frozen$path)
  if (!isTRUE(current$exists) || !identical(current$md5, frozen$md5) ||
      !identical(as.numeric(current$size), as.numeric(frozen$size)))
    stop("The pipeline source changed after run provenance was frozen. The run is invalid.",
         call. = FALSE)
  invisible(TRUE)
}


get_frozen_config_hash <- function(cfg, which = c("resolved", "analysis")) {
  which <- match.arg(which)
  p <- cfg$provenance %||% list()
  if (which == "analysis") p$analysis_spec_md5 %||% NA_character_ else
    p$resolved_run_config_md5 %||% NA_character_
}

# v6: wrapper around write.csv that uses build_unique_path and writes a
# metadata header describing the run configuration. This makes every CSV
# self-identifying when emailed or stored outside the run folder.
provenance_metadata_lines <- function(cfg, path, base_filename) {
  fp <- get_frozen_source_fingerprint(cfg)
  rt <- cfg$provenance$runtime %||% runtime_provenance()
  c(
    sprintf("# File:          %s", basename(path)),
    sprintf("# Generated:     %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    sprintf("# Base name:     %s", base_filename),
    sprintf("# Outcome:       family=%s, wave=%s, member=%s",
      cfg$outcome$family %||% "NA",
      as.character(cfg$outcome$current_wave %||% "NA"),
      cfg$outcome$family_member %||% "NA"),
    sprintf("# Run ID:        %s", cfg$global$run_id %||% ""),
    sprintf("# Pipeline seed: %s", cfg$global$pipeline_seed),
    sprintf("# Pipeline ver.: %s", cfg$global$version %||% ""),
    sprintf("# Script MD5:    %s", fp$md5),
    sprintf("# Analysis spec: %s", get_frozen_config_hash(cfg, "analysis")),
    sprintf("# Run config:    %s", get_frozen_config_hash(cfg, "resolved")),
    sprintf("# Run label:     %s", cfg$global$run_label %||% "NA"),
    sprintf("# Outcome def:   %s", cfg$provenance$outcome_definition %||%
      configured_outcome_definition(cfg)),
    sprintf("# R version:     %s", rt$R_version %||% runtime_provenance()$R_version),
    sprintf("# Package vers.: %s", rt$packages %||% runtime_provenance()$packages))
}

write_provenance_csv_at_path <- function(x, cfg, path, base_filename,
                                         row.names = FALSE, overwrite = NULL) {
  xx <- as.data.frame(x)
  if (ncol(xx) < 1L)
    stop("Refusing to write a zero-column provenance CSV artifact: ", path,
         call. = FALSE)
  assert_path_length_safe(path, cfg, "CSV output path")
  ensure_output_dir(dirname(path), cfg = cfg, test_write = FALSE)
  if (is.null(overwrite)) overwrite <- isTRUE(cfg$safety$allow_output_overwrite %||% FALSE)
  tf <- tempfile(pattern = paste0(".", basename(path), "_"),
                 tmpdir = dirname(path), fileext = ".tmp")
  on.exit(if (file.exists(tf)) unlink(tf, force = TRUE), add = TRUE)
  writeLines(provenance_metadata_lines(cfg, path, base_filename), con = tf,
             useBytes = TRUE)
  suppressWarnings(utils::write.table(
    xx, file = tf, sep = ",", row.names = row.names,
    col.names = TRUE, append = TRUE, qmethod = "double", na = ""))
  if (isTRUE(cfg$safety$verify_atomic_writes %||% TRUE)) {
    chk <- tryCatch(utils::read.csv(tf, stringsAsFactors = FALSE,
      check.names = FALSE, comment.char = "#"), error = function(e) e)
    if (inherits(chk, "error") || nrow(chk) != nrow(xx) || ncol(chk) != ncol(xx))
      stop("CSV verification failed before commit for: ", path, call. = FALSE)
  }
  atomic_commit_file(tf, path, overwrite = overwrite)
}

write_run_csv <- function(x, cfg, base_filename, row.names = FALSE) {
  path <- build_unique_path(cfg, base_filename)
  write_provenance_csv_at_path(x, cfg, path, base_filename,
                               row.names = row.names)
  invisible(path)
}


assert_file_exists <- function(path) {
  if (!file.exists(path)) stop("Required file does not exist: ", path, call. = FALSE)
}

assert_required_columns <- function(df, cols, df_name = deparse(substitute(df))) {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0L)
    stop(df_name, " is missing required column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
}

# Lightweight stopwatch used by the pipeline runner to report per-stage time.
make_timer_log <- function() {
  e <- new.env(parent = emptyenv()); e$starts <- list(); e$elapsed <- list()
  list(
    start = function(label) { e$starts[[label]] <- proc.time()[3]; invisible(NULL) },
    stop  = function(label) {
      if (is.null(e$starts[[label]])) return(invisible(NULL))
      dt <- proc.time()[3] - e$starts[[label]]
      e$elapsed[[label]] <- (e$elapsed[[label]] %||% 0) + dt
      e$starts[[label]]  <- NULL; invisible(dt)
    },
    get = function() {
      if (length(e$elapsed) == 0L)
        return(data.frame(stage = character(0), seconds = numeric(0)))
      data.frame(stage = names(e$elapsed), seconds = unlist(e$elapsed), row.names = NULL)
    }
  )
}

to_num <- function(x) {
  x_chr <- as.character(x); x_chr <- gsub("[, ]", "", x_chr)
  suppressWarnings(as.numeric(x_chr))
}


# Convert numeric-coded vectors without using factor level numbers. For a
# factor, as.numeric(factor) returns internal level indices; converting through
# character preserves the actual Add Health values.
to_numeric_codes <- function(x) {
  if (is.factor(x) || is.character(x))
    return(suppressWarnings(as.numeric(as.character(x))))
  suppressWarnings(as.numeric(x))
}


normalize_positive_weights <- function(w, n = NULL, label = "weights") {
  if (is.null(w)) {
    if (is.null(n)) stop(label, ": n is required when weights are NULL.", call. = FALSE)
    return(rep(1, n))
  }
  w <- as.numeric(w)
  if (!is.null(n) && length(w) != n)
    stop(label, ": length mismatch.", call. = FALSE)
  if (any(!is.finite(w)) || any(w <= 0))
    stop(label, ": weights must be positive and finite.", call. = FALSE)
  mw <- mean(w)
  if (!is.finite(mw) || mw <= 0)
    stop(label, ": invalid mean weight.", call. = FALSE)
  w / mw
}

matches_any_regex <- function(x, patterns) {
  if (is.null(x) || length(x) != 1L || is.na(x) || !nzchar(x) ||
      is.null(patterns) || !length(patterns)) return(FALSE)
  any(vapply(patterns, function(p) isTRUE(grepl(p, x, perl = TRUE)), logical(1)))
}

is_forced_factor_name <- function(variable_name, cfg_pre) {
  if (is.null(variable_name) || length(variable_name) != 1L || is.na(variable_name)) return(FALSE)
  variable_name %in% (cfg_pre$long_factors %||% character(0)) ||
    any(vapply(cfg_pre$force_factor_prefixes %||% character(0),
               function(p) startsWith(variable_name, p), logical(1)))
}

is_questionnaire_source_name <- function(variable_name, cfg_pre) {
  if (is.null(variable_name) || length(variable_name) != 1L || is.na(variable_name) || !nzchar(variable_name))
    return(FALSE)
  if (variable_name %in% (cfg_pre$explicit_questionnaire_vars %||% character(0))) return(TRUE)
  if (variable_name %in% (cfg_pre$nonquestionnaire_long_factors %||% character(0))) return(FALSE)
  if (matches_any_regex(variable_name, cfg_pre$questionnaire_name_exclude_patterns %||% character(0))) return(FALSE)
  matches_any_regex(variable_name, cfg_pre$questionnaire_name_patterns %||% character(0))
}

# Generate exact conventional Add Health-style special-code families. There is
# deliberately NO suffix matching: 196 is not code 96, and 22,198 is not 98.
conventional_special_code_families <- function(cfg_pre) {
  max_digits <- as.integer(cfg_pre$auto_special_code_max_digits %||% 7L)
  max_digits <- max(2L, min(max_digits, 7L))
  out <- list(
    list(name = "low_1_digit",
         skip = as.numeric(cfg_pre$factor_skip_code_low %||% 7L),
         general = as.numeric(cfg_pre$factor_refusal_codes_low %||% c(6L, 8L, 9L)),
         digits = 1L),
    list(name = "high_2_digit",
         skip = as.numeric(cfg_pre$factor_skip_code_high %||% 97L),
         general = as.numeric(cfg_pre$factor_refusal_codes_high %||% c(96L, 98L, 99L)),
         digits = 2L)
  )
  if (max_digits >= 3L) {
    for (d in 3L:max_digits) {
      prefix <- paste(rep("9", d - 2L), collapse = "")
      codes <- suppressWarnings(as.numeric(paste0(prefix, 96:99)))
      out[[length(out) + 1L]] <- list(
        name = paste0("high_", d, "_digit"),
        skip = codes[2L], general = codes[c(1L, 3L, 4L)], digits = d)
    }
  }
  out
}

# Native missingness includes NA for every storage type and blank/whitespace-only
# values for character or factor fields. Blank text is not a substantive factor
# level and must remain missing throughout dictionary construction and fold recipes.
character_native_missing_mask <- function(x) {
  if (is.factor(x) || is.character(x)) {
    xc <- trimws(as.character(x))
    return(is.na(x) | is.na(xc) | !nzchar(xc))
  }
  if (is.numeric(x) || is.integer(x))
    return(is.na(x) | !is.finite(as.numeric(x)))
  is.na(x)
}

canonicalize_factor_text <- function(x) {
  xc <- trimws(as.character(x))
  xc[character_native_missing_mask(x)] <- NA_character_
  xc
}

assert_reserved_factor_labels_safe <- function(x, general_mask, skip_mask,
                                                reserved_labels, variable_name,
                                                context) {
  xc <- canonicalize_factor_text(x)
  substantive <- !(general_mask | skip_mask) & !is.na(xc)
  collision <- sort(unique(xc[substantive & xc %in% reserved_labels]))
  if (length(collision)) {
    stop(sprintf(
      "%s: raw variable '%s' contains substantive value(s) reserved for generated factor states: %s. Change the configured labels or explicitly recode the raw field before modeling.",
      context, variable_name, paste(collision, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

# Validate that derived numeric missingness-indicator names cannot overwrite or
# be overwritten by raw variables. The check runs before both the rough-screen
# and final-W recipes create any *_missA or *_miss97 columns.
assert_no_missing_indicator_name_collisions <- function(raw_names, numeric_names,
                                                        context = "missingness preprocessing") {
  raw_names <- unique(as.character(raw_names))
  numeric_names <- unique(intersect(as.character(numeric_names), raw_names))
  derived <- unlist(lapply(numeric_names, function(nm)
    c(paste0(nm, "_missA"), paste0(nm, "_miss97"))), use.names = FALSE)
  duplicate_derived <- unique(derived[duplicated(derived)])
  colliding_raw <- intersect(derived, raw_names)
  if (length(duplicate_derived) || length(colliding_raw)) {
    details <- c(
      if (length(colliding_raw)) paste0("raw/derived collisions: ", paste(colliding_raw, collapse = ", ")),
      if (length(duplicate_derived)) paste0("duplicate derived names: ", paste(duplicate_derived, collapse = ", "))
    )
    stop(sprintf(
      "%s cannot safely create numeric missingness indicators (%s). Rename or explicitly exclude the colliding raw variable(s).",
      context, paste(details, collapse = "; ")), call. = FALSE)
  }
  invisible(TRUE)
}

# Learn an exact-code rule from the complete outcome-blind Wave I support.
# The rule is source-informed but automated: questionnaire variable-name structure and the
# analyst's existing long-factor declarations determine where Add Health's
# questionnaire code conventions are plausible; observed support chooses the
# low (6/7/8/9), high (96/97/98/99), or wider exact family. Frequency is never
# used to reject a code, because legitimate skips can be the majority of rows.
learn_conservative_missing_rule <- function(x, cfg_pre, variable_name = NULL) {
  raw_missing <- character_native_missing_mask(x)
  x_for_parse <- x
  if (is.factor(x_for_parse) || is.character(x_for_parse)) {
    x_for_parse <- trimws(as.character(x_for_parse))
    x_for_parse[raw_missing] <- NA_character_
  }
  z <- to_numeric_codes(x_for_parse)
  nonmissing <- !raw_missing
  parsed <- nonmissing & is.finite(z)
  original_factor <- is.factor(x) || is.character(x)
  forced_factor <- is_forced_factor_name(variable_name, cfg_pre)
  questionnaire_source <- is_questionnaire_source_name(variable_name, cfg_pre)

  # Fully nonnumeric character/factor variables are valid categorical fields.
  # They use native NA only because numeric Add Health reserve-code families
  # cannot be inferred from text labels. A mixture of numeric and nonnumeric
  # nonmissing values is treated as a malformed field and stops loudly.
  if (any(nonmissing) && !any(parsed)) {
    vals <- trimws(as.character(x[nonmissing]))
    return(list(
      classifier = "global_source_informed_exact_v1", numeric_coded = FALSE,
      support_type = "nonnumeric_categorical", questionnaire_source = questionnaire_source,
      forced_factor = forced_factor, questionnaire_like = questionnaire_source,
      categorical_questionnaire = questionnaire_source, percentage_like = FALSE,
      dense_small_count = FALSE, as_factor = TRUE,
      general_codes = numeric(0), skip_codes = numeric(0),
      recognized_families = character(0), scheme_decision = "text_native_missing_only",
      reason = "nonnumeric_labels_native_missing_only", n_observed_raw = sum(nonmissing),
      n_finite = sum(nonmissing), n_unique_finite = length(unique(vals)),
      n_unique_substantive = length(unique(vals)), integer_like_prop = NA_real_))
  }
  bad_parse <- nonmissing & !is.finite(z)
  if (any(bad_parse)) {
    examples <- unique(as.character(x[bad_parse]))
    examples <- examples[seq_len(min(length(examples), 5L))]
    stop(sprintf(
      "Missing-code dictionary: variable %s mixes numeric codes with %d nonnumeric nonmissing value(s) (examples: %s). Exclude or explicitly transform this malformed mixed-type field.",
      variable_name %||% "<unnamed>", sum(bad_parse), paste(examples, collapse = ", ")),
      call. = FALSE)
  }

  empty_rule <- list(
    classifier = "global_source_informed_exact_v1",
    numeric_coded = !(original_factor && !any(nonmissing)),
    support_type = "all_missing", questionnaire_source = questionnaire_source,
    forced_factor = forced_factor, questionnaire_like = FALSE,
    categorical_questionnaire = FALSE, percentage_like = FALSE,
    dense_small_count = FALSE, as_factor = forced_factor || original_factor,
    general_codes = numeric(0), skip_codes = numeric(0),
    recognized_families = character(0), scheme_decision = "none",
    reason = "no_exact_special_code_family", n_observed_raw = sum(nonmissing),
    n_finite = sum(parsed), n_unique_finite = 0L, n_unique_substantive = 0L,
    integer_like_prop = NA_real_)
  if (!any(parsed)) return(empty_rule)

  zf <- z[parsed]
  tol <- cfg_pre$auto_integer_tolerance %||% 1e-8
  integer_like <- abs(zf - round(zf)) <= tol * pmax(1, abs(zf))
  integer_like_prop <- mean(integer_like)
  n_unique <- length(unique(zf))

  families <- conventional_special_code_families(cfg_pre)
  low_fam <- families[[1L]]
  high_fam <- families[[2L]]
  all_candidate_codes <- unique(unlist(lapply(families, function(f) c(f$general, f$skip))))
  substantive_all <- zf[!(zf %in% all_candidate_codes)]
  n_unique_substantive <- length(unique(substantive_all))
  unique_prop_substantive <- if (length(substantive_all))
    n_unique_substantive / length(substantive_all) else 0

  wide_codes <- unique(unlist(lapply(
    families[vapply(families, function(f) f$digits >= 3L, logical(1))],
    function(f) c(f$general, f$skip))))
  z_no_wide <- zf[!(zf %in% wide_codes)]
  pct_min <- if (length(z_no_wide)) min(z_no_wide) else NA_real_
  pct_max <- if (length(z_no_wide)) max(z_no_wide) else NA_real_
  pct_n_unique <- length(unique(z_no_wide))
  percentage_like <-
    integer_like_prop >= (cfg_pre$auto_integer_like_min_prop %||% 0.99) &&
    is.finite(pct_min) && is.finite(pct_max) && pct_min >= 0 && pct_max <= 100 &&
    pct_n_unique >= (cfg_pre$auto_percentage_min_unique %||% 20L) &&
    (pct_max - pct_min) >= (cfg_pre$auto_percentage_min_span %||% 50)

  z_unique_int <- sort(unique(round(zf[integer_like])))
  dense_small_count <- FALSE
  if (length(z_unique_int)) {
    lo <- min(z_unique_int); hi <- max(z_unique_int)
    dense_small_count <- lo >= 0 &&
      hi <= (cfg_pre$auto_dense_small_count_max_value %||% 20L) &&
      length(z_unique_int) >= (cfg_pre$auto_dense_small_count_min_unique %||% 8L) &&
      identical(z_unique_int, seq.int(lo, hi))
  }

  max_abs_substantive <- if (length(substantive_all)) max(abs(substantive_all)) else 0
  auto_categorical <-
    integer_like_prop >= (cfg_pre$auto_integer_like_min_prop %||% 0.99) &&
    max_abs_substantive <= (cfg_pre$auto_categorical_max_abs_value %||% 100) &&
    (n_unique_substantive <= (cfg_pre$factor_unique_threshold %||% 10L) ||
       (n_unique_substantive <= (cfg_pre$auto_questionnaire_max_unique %||% 30L) &&
          unique_prop_substantive <= (cfg_pre$auto_questionnaire_max_unique_prop %||% 0.01)))
  if (dense_small_count && !questionnaire_source && !forced_factor && !original_factor)
    auto_categorical <- FALSE

  categorical_questionnaire <-
    (questionnaire_source && (forced_factor || original_factor || auto_categorical)) ||
    (forced_factor && !(variable_name %in% (cfg_pre$nonquestionnaire_long_factors %||% character(0))))
  questionnaire_like <- questionnaire_source || categorical_questionnaire
  as_factor <- forced_factor || original_factor || auto_categorical

  general_codes <- numeric(0); skip_codes <- numeric(0)
  recognized <- character(0); reasons <- character(0)
  scheme_decision <- "none"

  obs_low <- intersect(unique(zf), c(low_fam$general, low_fam$skip))
  obs_high <- intersect(unique(zf), c(high_fam$general, high_fam$skip))

  # Wider exact field-width families take precedence. Only the exact family is
  # considered: 19,996 is never treated as code 9,996.
  wide_families <- families[vapply(families, function(f) f$digits >= 3L, logical(1))]
  observed_wide_families <- wide_families[vapply(
    wide_families, function(f) any(zf %in% c(f$general, f$skip)), logical(1))]
  chosen_wide <- NULL
  if ((questionnaire_source || categorical_questionnaire) && length(observed_wide_families)) {
    observed_wide_families <- observed_wide_families[
      order(vapply(observed_wide_families, function(f) f$digits, integer(1)), decreasing = TRUE)]
    for (fam in observed_wide_families) {
      fam_codes <- c(fam$general, fam$skip)
      ordinary_without_family <- zf[!(zf %in% fam_codes)]
      # A variable can be entirely skipped/nonresponding in the complete Wave I
      # support. In that case an observed exact family is still a supported
      # semantic family; there need not be an ordinary value below it.
      if (!length(ordinary_without_family) ||
          max(ordinary_without_family) < min(fam_codes)) {
        chosen_wide <- fam
        break
      }
    }
  }
  has_supported_wide <- !is.null(chosen_wide)

  # For low-versus-high inference, 7/8/9 are decisive evidence for the
  # low scheme. Exact 6 alone is not: Add Health items with substantive category
  # 6 use 96-99 for nonresponse, so 6 is retained when 7/8/9 are absent.
  obs_low_decisive <- intersect(unique(zf), c(7, 8, 9))
  base_low_high <- zf[!(zf %in% c(7, 8, 9,
                                  high_fam$general, high_fam$skip))]
  max_base <- if (length(base_low_high)) max(base_low_high) else NA_real_
  chosen <- NULL
  bounded_measure_protected <- percentage_like && !forced_factor && !original_factor
  if (!has_supported_wide && categorical_questionnaire && !bounded_measure_protected) {
    if (length(obs_high)) {
      chosen <- high_fam
      scheme_decision <- "high_observed_96_99"
    } else if (length(obs_low_decisive)) {
      chosen <- low_fam
      scheme_decision <- "low_observed_7_8_9"
    } else if (is.finite(max_base) && max_base <= 5) {
      chosen <- low_fam
      scheme_decision <- "low_inferred_substantive_max_le_5"
    } else if (is.finite(max_base)) {
      chosen <- high_fam
      scheme_decision <- "high_inferred_substantive_category_6_or_higher"
    }
  } else if (!has_supported_wide && questionnaire_source &&
             !percentage_like && length(obs_high)) {
    ordinary_without_high <- zf[!(zf %in% c(high_fam$general, high_fam$skip))]
    if (length(ordinary_without_high) && max(ordinary_without_high) < 96) {
      chosen <- high_fam
      scheme_decision <- "high_numeric_field_width_above_ordinary_support"
    }
  }
  if (!is.null(chosen)) {
    general_codes <- c(general_codes, chosen$general)
    skip_codes <- c(skip_codes, chosen$skip)
    recognized <- c(recognized, chosen$name)
    reasons <- c(reasons, scheme_decision)
  }
  if (!is.null(chosen_wide)) {
    general_codes <- c(general_codes, chosen_wide$general)
    skip_codes <- c(skip_codes, chosen_wide$skip)
    recognized <- c(recognized, chosen_wide$name)
    reasons <- c(reasons, paste0(chosen_wide$name, "_widest_observed_above_ordinary_support"))
  }

  if (!is.null(variable_name) &&
      variable_name %in% (cfg_pre$known_codebook_overlap_vars %||% character(0)) &&
      identical(cfg_pre$known_overlap_policy %||% "preserve_as_substantive",
                "preserve_as_substantive")) {
    general_codes <- numeric(0)
    skip_codes <- numeric(0)
    recognized <- character(0)
    scheme_decision <- "known_overlap_preserved"
    reasons <- c(reasons, "known_codebook_overlap_preserved_as_substantive")
  }

  support_type <- if (categorical_questionnaire) "categorical_questionnaire" else
    if (questionnaire_source) "questionnaire_numeric_or_count" else
      "continuous_contextual_or_other"

  list(
    classifier = "global_source_informed_exact_v1", numeric_coded = TRUE,
    support_type = support_type, questionnaire_source = questionnaire_source,
    forced_factor = forced_factor, questionnaire_like = questionnaire_like,
    categorical_questionnaire = categorical_questionnaire,
    percentage_like = percentage_like, dense_small_count = dense_small_count,
    as_factor = as_factor,
    general_codes = sort(unique(general_codes)),
    skip_codes = sort(unique(skip_codes)),
    recognized_families = unique(recognized), scheme_decision = scheme_decision,
    reason = if (length(reasons)) paste(unique(reasons), collapse = ";") else
      "no_exact_special_code_family",
    n_observed_raw = sum(!raw_missing), n_finite = length(zf),
    n_unique_finite = n_unique, n_unique_substantive = n_unique_substantive,
    integer_like_prop = integer_like_prop)
}


make_native_only_missing_rule <- function(x, variable_name = NULL, reason = "derived_or_unregistered_variable") {
  raw_missing <- character_native_missing_mask(x)
  x_for_parse <- x
  if (is.factor(x_for_parse) || is.character(x_for_parse)) {
    x_for_parse <- trimws(as.character(x_for_parse))
    x_for_parse[raw_missing] <- NA_character_
  }
  z <- to_numeric_codes(x_for_parse)
  nonmissing <- !raw_missing
  parsed <- nonmissing & is.finite(z)
  mixed <- any(parsed) && any(nonmissing & !is.finite(z))
  if (mixed)
    stop("Native-only missing rule received a mixed numeric/nonnumeric field for ",
         variable_name %||% "<unnamed>", ".", call. = FALSE)
  numeric_coded <- !any(nonmissing) || any(parsed)
  vals <- if (numeric_coded) z[parsed] else trimws(as.character(x[nonmissing]))
  list(
    classifier = "global_source_informed_exact_v1", numeric_coded = numeric_coded,
    support_type = "derived_or_unregistered", questionnaire_source = FALSE,
    forced_factor = FALSE, questionnaire_like = FALSE,
    categorical_questionnaire = FALSE, percentage_like = FALSE,
    dense_small_count = FALSE, as_factor = !numeric_coded || is.factor(x) || is.character(x),
    general_codes = numeric(0), skip_codes = numeric(0),
    recognized_families = character(0), scheme_decision = "none",
    reason = reason, n_observed_raw = sum(nonmissing),
    n_finite = length(vals), n_unique_finite = length(unique(vals)),
    n_unique_substantive = length(unique(vals)),
    integer_like_prop = if (numeric_coded && length(vals))
      mean(abs(vals - round(vals)) <= 1e-8 * pmax(1, abs(vals))) else NA_real_)
}

build_global_missing_code_dictionary <- function(df, vars, cfg_pre) {
  vars <- unique(vars[vars %in% names(df)])
  if (!length(vars)) stop("Global missing-code dictionary has no variables.", call. = FALSE)
  out <- setNames(vector("list", length(vars)), vars)
  for (i in seq_along(vars)) {
    nm <- vars[[i]]
    out[[nm]] <- learn_conservative_missing_rule(df[[nm]], cfg_pre, variable_name = nm)
  }
  attr(out, "dictionary_version") <- "global_source_informed_exact_v1_blank_aware"
  attr(out, "n_rows_used") <- nrow(df)
  out
}

get_missing_rule <- function(x, cfg_pre, variable_name = NULL) {
  dict <- cfg_pre$global_missing_dictionary
  if (!is.null(dict)) {
    if (!is.null(variable_name) && variable_name %in% names(dict))
      return(dict[[variable_name]])
    native_patterns <- cfg_pre$global_missing_dictionary_native_only_patterns %||% character(0)
    is_derived_native <- !is.null(variable_name) &&
      matches_any_regex(variable_name, native_patterns)
    if (is_derived_native) {
      return(make_native_only_missing_rule(
        x, variable_name,
        reason = "explicit_derived_variable_native_missing_only"))
    }
    if (isTRUE(cfg_pre$global_missing_dictionary_required %||% TRUE)) {
      stop(
        "Frozen missing-code dictionary has no rule for Wave I candidate '",
        variable_name %||% "<unnamed>",
        "'. Rebuild the Wave I/main-data caches; do not learn semantic codes inside a fold.",
        call. = FALSE)
    }
    return(make_native_only_missing_rule(
      x, variable_name,
      reason = "unregistered_variable_native_missing_only"))
  }
  # Synthetic unit-test fallback only. Production runner injects the frozen
  # dictionary before any prefilter, screen, or final-W recipe is created.
  learn_conservative_missing_rule(x, cfg_pre, variable_name = variable_name)
}

missing_dictionary_to_data_frame <- function(dictionary, df = NULL) {
  if (is.null(dictionary) || !length(dictionary)) return(data.frame())
  rows <- lapply(names(dictionary), function(nm) {
    r <- dictionary[[nm]]
    x <- if (!is.null(df) && nm %in% names(df)) df[[nm]] else NULL
    masks <- if (!is.null(x)) missing_masks_from_rule(x, r) else NULL
    data.frame(
      variable = nm,
      classifier = r$classifier %||% NA_character_,
      support_type = r$support_type %||% NA_character_,
      as_factor = isTRUE(r$as_factor),
      questionnaire_source = isTRUE(r$questionnaire_source),
      forced_factor = isTRUE(r$forced_factor),
      percentage_like = isTRUE(r$percentage_like),
      dense_small_count = isTRUE(r$dense_small_count),
      recognized_families = paste(r$recognized_families %||% character(0), collapse = ";"),
      exact_general_codes = paste(r$general_codes %||% numeric(0), collapse = ";"),
      exact_skip_codes = paste(r$skip_codes %||% numeric(0), collapse = ";"),
      scheme_decision = r$scheme_decision %||% NA_character_,
      reason = r$reason %||% NA_character_,
      n_rows_dictionary = attr(dictionary, "n_rows_used") %||% NA_integer_,
      n_observed_raw = r$n_observed_raw %||% NA_integer_,
      n_unique_finite = r$n_unique_finite %||% NA_integer_,
      n_unique_substantive = r$n_unique_substantive %||% NA_integer_,
      n_general_flagged = if (is.null(masks)) NA_integer_ else sum(masks$general),
      n_skip_flagged = if (is.null(masks)) NA_integer_ else sum(masks$skip),
      stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

missing_masks_from_rule <- function(x, rule) {
  native_missing <- character_native_missing_mask(x)
  x_for_parse <- x
  if (is.factor(x_for_parse) || is.character(x_for_parse)) {
    x_for_parse <- trimws(as.character(x_for_parse))
    x_for_parse[native_missing] <- NA_character_
  }
  z <- to_numeric_codes(x_for_parse)
  if (isTRUE(rule$numeric_coded)) native_missing <- native_missing | !is.finite(z)
  finite_code <- isTRUE(rule$numeric_coded) & is.finite(z)
  miss_skip <- finite_code & z %in% (rule$skip_codes %||% numeric(0))
  miss_general <- native_missing |
    (finite_code & z %in% (rule$general_codes %||% numeric(0)))
  list(general = as.logical(miss_general), skip = as.logical(miss_skip), numeric = z)
}

compute_simple_impute <- function(x, method = c("median","mean")) {
  method <- match.arg(method)
  x <- suppressWarnings(as.numeric(x)); x_ok <- x[is.finite(x)]
  if (!length(x_ok)) return(0)
  val <- if (method == "median") stats::median(x_ok, na.rm = TRUE) else mean(x_ok, na.rm = TRUE)
  if (!is.finite(val)) 0 else val
}

scale_numeric_train_valid <- function(train_df, valid_df) {
  common   <- intersect(names(train_df), names(valid_df))
  num_cols <- common[vapply(train_df[common], is.numeric, logical(1))]
  if (!length(num_cols)) return(list(train = train_df, valid = valid_df))
  for (nm in num_cols) {
    mu <- mean(train_df[[nm]], na.rm = TRUE); s <- stats::sd(train_df[[nm]], na.rm = TRUE)
    if (!is.finite(mu)) mu <- 0
    if (!is.finite(s) || s <= 0) s <- 1
    train_df[[nm]] <- (train_df[[nm]] - mu) / s
    valid_df[[nm]] <- (valid_df[[nm]] - mu) / s
  }
  list(train = train_df, valid = valid_df)
}

read_xpt_df <- function(path) {
  assert_file_exists(path)
  data.frame(haven::read_xpt(path), check.names = FALSE)
}

coerce_join_key <- function(df, key, label = deparse(substitute(df)),
                            require_complete = FALSE) {
  if (!key %in% names(df)) stop(label, " is missing join key ", key, ".", call. = FALSE)
  raw <- df[[key]]
  raw_chr <- trimws(as.character(raw))
  was_missing <- is.na(raw) | is.na(raw_chr) | raw_chr == ""
  converted <- if (is.numeric(raw) || is.integer(raw)) {
    suppressWarnings(as.numeric(raw))
  } else {
    suppressWarnings(as.numeric(raw_chr))
  }
  newly_invalid <- !was_missing & (!is.finite(converted) | is.na(converted))
  if (any(newly_invalid)) {
    examples <- unique(raw_chr[newly_invalid])
    examples <- examples[seq_len(min(length(examples), 5L))]
    stop(sprintf(
      "Join-key coercion failed: %s has %d nonmissing %s value(s) that are not finite numeric keys (examples: %s).",
      label, sum(newly_invalid), key, paste(examples, collapse = ", ")), call. = FALSE)
  }
  converted[was_missing] <- NA_real_
  df[[key]] <- converted
  if (isTRUE(require_complete) && anyNA(df[[key]])) {
    stop(sprintf("Join-cardinality check failed: master table %s has %d missing %s value(s).",
                 label, sum(is.na(df[[key]])), key), call. = FALSE)
  }
  df
}

assert_unique_key <- function(df, key, label = deparse(substitute(df)),
                              require_complete = FALSE) {
  if (!key %in% names(df)) stop(label, " is missing join key ", key, ".", call. = FALSE)
  if (isTRUE(require_complete) && anyNA(df[[key]])) {
    stop(sprintf("Join-cardinality check failed: master table %s has %d missing %s value(s).",
                 label, sum(is.na(df[[key]])), key), call. = FALSE)
  }
  key_nonmissing <- df[[key]][!is.na(df[[key]])]
  dup_n <- sum(duplicated(key_nonmissing) |
                 duplicated(key_nonmissing, fromLast = TRUE))
  if (dup_n > 0L) {
    stop(sprintf("Join-cardinality check failed: %s has %d rows with duplicated nonmissing %s values.",
                 label, dup_n, key), call. = FALSE)
  }
  invisible(TRUE)
}

left_join_unique <- function(x, y, key, x_label = "left table", y_label = "right table",
                             require_complete_x = TRUE, require_complete_y = FALSE) {
  x <- coerce_join_key(x, key, x_label, require_complete = require_complete_x)
  y <- coerce_join_key(y, key, y_label, require_complete = require_complete_y)
  assert_unique_key(x, key, x_label, require_complete = require_complete_x)
  assert_unique_key(y, key, y_label, require_complete = require_complete_y)
  n_before <- nrow(x)
  out <- dplyr::left_join(x, y, by = key, na_matches = "never")
  out_nonmissing <- out[[key]][!is.na(out[[key]])]
  if (nrow(out) != n_before || anyDuplicated(out_nonmissing) > 0L) {
    stop(sprintf("Join-cardinality check failed for %s <- %s on %s: rows %d -> %d or duplicate nonmissing keys created.",
                 x_label, y_label, key, n_before, nrow(out)), call. = FALSE)
  }
  message(sprintf("  [join audit] %s <- %s on %s (one-to-one): %d rows, %d nonmissing unique keys, %d missing left keys, %d missing right keys; no row multiplication.",
                  x_label, y_label, key, nrow(out), length(unique(out_nonmissing)),
                  sum(is.na(out[[key]])), sum(is.na(y[[key]]))))
  out
}

left_join_many_to_one <- function(x, y, key, x_label = "left table", y_label = "right table",
                                  row_id = NULL) {
  x <- coerce_join_key(x, key, x_label, require_complete = FALSE)
  y <- coerce_join_key(y, key, y_label, require_complete = FALSE)
  assert_unique_key(y, key, y_label, require_complete = FALSE)
  n_before <- nrow(x)
  if (!is.null(row_id)) {
    if (!row_id %in% names(x)) stop(x_label, " is missing row identifier ", row_id, ".", call. = FALSE)
    x <- coerce_join_key(x, row_id, x_label, require_complete = TRUE)
    assert_unique_key(x, row_id, x_label, require_complete = TRUE)
    row_order <- x[[row_id]]
  } else {
    row_order <- seq_len(n_before)
    x$.join_row_order_internal <- row_order
  }
  out <- dplyr::left_join(x, y, by = key, na_matches = "never")
  if (nrow(out) != n_before) {
    stop(sprintf("Join-cardinality check failed for %s <- %s on %s: rows %d -> %d.",
                 x_label, y_label, key, n_before, nrow(out)), call. = FALSE)
  }
  out_order <- if (!is.null(row_id)) out[[row_id]] else out$.join_row_order_internal
  if (!identical(as.character(out_order), as.character(row_order)))
    stop(sprintf("Join-cardinality check failed for %s <- %s on %s: left-row order changed.",
                 x_label, y_label, key), call. = FALSE)
  if (is.null(row_id)) out$.join_row_order_internal <- NULL
  message(sprintf("  [join audit] %s <- %s on %s (many-to-one): %d left rows, %d nonmissing left key values, %d missing left keys, %d nonmissing right keys, %d missing right keys; no row multiplication.",
                  x_label, y_label, key, nrow(out),
                  length(unique(x[[key]][!is.na(x[[key]])])), sum(is.na(x[[key]])),
                  length(unique(y[[key]][!is.na(y[[key]])])), sum(is.na(y[[key]]))))
  out
}

merge_by_key <- function(dfs, key, labels = NULL) {
  if (length(dfs) == 0L) stop("merge_by_key requires at least one data frame.", call. = FALSE)
  if (is.null(labels)) labels <- paste0("table_", seq_along(dfs))
  if (length(labels) != length(dfs)) stop("merge_by_key labels length mismatch.", call. = FALSE)
  out <- coerce_join_key(dfs[[1L]], key, labels[[1L]], require_complete = TRUE)
  assert_unique_key(out, key, labels[[1L]], require_complete = TRUE)
  if (length(dfs) > 1L) {
    for (i in 2:length(dfs)) {
      out <- left_join_unique(
        out, dfs[[i]], key,
        x_label = paste(labels[seq_len(i - 1L)], collapse = "+"),
        y_label = labels[[i]],
        require_complete_x = TRUE,
        require_complete_y = FALSE)
    }
  }
  out
}

# Safely resolve dplyr .x/.y collisions. Agreeing pairs are coalesced;
# conflicting, type-mismatched, orphan, and no-overlap pairs are retained.
join_value_observed <- function(x) {
  if (is.numeric(x) || is.integer(x)) return(is.finite(as.numeric(x)))
  !character_native_missing_mask(x)
}

join_pair_conflicts <- function(x, y, overlap, tol = 0) {
  out <- rep(FALSE, length(overlap))
  if (!any(overlap)) return(out)
  if ((is.numeric(x) || is.integer(x)) && (is.numeric(y) || is.integer(y))) {
    xv <- as.numeric(x); yv <- as.numeric(y)
    scale <- pmax(1, abs(xv), abs(yv))
    out[overlap] <- abs(xv[overlap] - yv[overlap]) > tol * scale[overlap]
  } else {
    xv <- trimws(as.character(x))
    yv <- trimws(as.character(y))
    out[overlap] <- xv[overlap] != yv[overlap]
    out[is.na(out)] <- TRUE
  }
  out
}

coalesce_join_pair <- function(x, y, fill_from_y) {
  if (!any(fill_from_y)) return(x)
  if ((is.numeric(x) || is.integer(x)) && (is.numeric(y) || is.integer(y))) {
    out <- as.numeric(x); out[fill_from_y] <- as.numeric(y)[fill_from_y]; return(out)
  }
  if (is.character(x) && is.character(y)) {
    out <- x; out[fill_from_y] <- y[fill_from_y]; return(out)
  }
  if (is.logical(x) && is.logical(y)) {
    out <- x; out[fill_from_y] <- y[fill_from_y]; return(out)
  }
  if (is.factor(x) && is.factor(y)) {
    out <- as.character(x); out[fill_from_y] <- as.character(y)[fill_from_y]
    lev <- unique(c(levels(x), levels(y), out[!is.na(out)]))
    return(factor(out, levels = lev))
  }
  NULL
}

resolve_join_suffix_collisions <- function(df, tol = 0) {
  y_cols <- grep("\\.y$", names(df), value = TRUE)
  audit_rows <- list(); drop_cols <- character(0)
  if (!length(y_cols)) return(list(data = df, audit = data.frame()))
  for (y_nm in y_cols) {
    base_nm <- sub("\\.y$", "", y_nm)
    x_nm <- paste0(base_nm, ".x")
    action <- "retained_orphan_y"
    n_x_obs <- n_y_obs <- n_overlap <- n_conflict <- n_y_only <- n_filled <- 0L
    examples <- ""
    if (x_nm %in% names(df)) {
      x <- df[[x_nm]]; y <- df[[y_nm]]
      obs_x <- join_value_observed(x); obs_y <- join_value_observed(y)
      overlap <- obs_x & obs_y
      conflict <- join_pair_conflicts(x, y, overlap, tol = tol)
      n_x_obs <- sum(obs_x); n_y_obs <- sum(obs_y)
      n_overlap <- sum(overlap); n_conflict <- sum(conflict)
      fill_from_y <- !obs_x & obs_y; n_y_only <- sum(fill_from_y)
      if (n_conflict > 0L) {
        ex_idx <- which(conflict)[seq_len(min(n_conflict, 5L))]
        examples <- paste(sprintf("row%s:%s!=%s", ex_idx,
                                  as.character(x[ex_idx]), as.character(y[ex_idx])),
                          collapse = " | ")
        action <- "retained_conflicting_pair"
      } else if (n_y_obs == 0L) {
        drop_cols <- c(drop_cols, y_nm); action <- "dropped_all_missing_y"
      } else if (n_x_obs == 0L) {
        merged <- coalesce_join_pair(x, y, fill_from_y)
        if (is.null(merged)) action <- "retained_type_mismatch" else {
          df[[x_nm]] <- merged; drop_cols <- c(drop_cols, y_nm)
          n_filled <- n_y_only
          action <- "replaced_all_missing_x_from_y"
        }
      } else if (n_overlap > 0L) {
        merged <- coalesce_join_pair(x, y, fill_from_y)
        if (is.null(merged)) action <- "retained_type_mismatch" else {
          df[[x_nm]] <- merged; drop_cols <- c(drop_cols, y_nm)
          n_filled <- n_y_only
          action <- "coalesced_agreeing_pair"
        }
      } else {
        action <- "retained_no_overlap_to_verify"
      }
    } else {
      n_y_obs <- sum(join_value_observed(df[[y_nm]]))
    }
    audit_rows[[length(audit_rows) + 1L]] <- data.frame(
      base_name = base_nm, x_column = if (x_nm %in% names(df)) x_nm else NA_character_,
      y_column = y_nm, n_x_observed = n_x_obs, n_y_observed = n_y_obs,
      n_overlap = n_overlap, n_conflicts = n_conflict,
      n_y_only = n_y_only, n_filled_from_y = n_filled, action = action,
      conflict_examples = examples, stringsAsFactors = FALSE)
  }
  if (length(drop_cols)) df <- df[, !names(df) %in% unique(drop_cols), drop = FALSE]
  list(data = df, audit = do.call(rbind, audit_rows))
}

safe_remove_cols <- function(df, pattern) {
  idx <- grep(pattern, names(df))
  if (length(idx) > 0L) df <- df[, -idx, drop = FALSE]
  df
}

remove_constant_columns <- function(df, tol = 1e-10, verbose = TRUE) {
  if (ncol(df) == 0L) return(df)
  is_constant <- vapply(df, function(col) {
    col_clean <- col[!is.na(col)]
    if (length(col_clean) < 2L) return(TRUE)
    if (is.factor(col_clean)) return(length(unique(col_clean)) < 2L)
    if (is.numeric(col_clean) || is.integer(col_clean)) {
      vv <- stats::var(as.numeric(col_clean))
      return(!is.finite(vv) || vv < tol)
    }
    length(unique(col_clean)) < 2L
  }, logical(1))
  n_const <- sum(is_constant)
  if (n_const > 0L && isTRUE(verbose))
    message(sprintf("[Safety] Removing %d constant columns", n_const))
  df[, !is_constant, drop = FALSE]
}

safe_scale <- function(x, eps = 1e-12) {
  if (!(is.numeric(x) || is.integer(x))) return(x)
  x <- as.numeric(x); x[!is.finite(x)] <- NA_real_
  s <- stats::sd(x, na.rm = TRUE)
  if (is.na(s) || s < eps) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

# Keep only letters, digits, and underscores in a single atomic string piece.
sanitize_piece <- function(x) {
  x <- as.character(x); x[is.na(x)] <- "NA"
  x <- gsub("[^A-Za-z0-9]+", "_", x); x <- gsub("^_+|_+$", "", x)
  x[x == ""] <- "blank"; x
}

# Sanitizes processed design-matrix column names. Special characters in
# column names (dots, spaces, parens) otherwise leak into SL formulas and
# produce inscrutable errors.
sanitize_column_names <- function(df) {
  clean_names <- gsub("[^A-Za-z0-9_]", "_", names(df))
  clean_names <- make.unique(clean_names)
  old_to_new  <- setNames(clean_names, names(df))
  names(df)   <- clean_names
  list(data = df, old_to_new = old_to_new)
}

# v5.1: Convert all factor columns to dummy (indicator) variables so that
# every learner in the SuperLearner library receives pure numeric input.
# Without this, as.matrix on a mixed data.frame coerces the entire matrix
# to character, which crashes xgboost and inflates memory by ~10x.
# Factors are expanded one column at a time rather than via a single
# model.matrix(~ ., data) call, because the latter overflows R's protect
# stack when the data.frame has hundreds of factor columns.
expand_factors_to_numeric <- function(df) {
  fac_cols <- names(df)[vapply(df, is.factor, logical(1))]
  if (length(fac_cols) == 0L) return(df)
  num_part <- df[, !names(df) %in% fac_cols, drop = FALSE]
  dummy_parts <- list()
  for (fn in fac_cols) {
    f <- df[[fn]]
    levs <- levels(f)
    if (length(levs) < 2L) next
    # Treatment coding: drop first level as reference to avoid collinearity
    # in GLM-based learners. Tree-based learners (ranger, xgboost) are
    # unaffected by the choice of reference level.
    mm_full <- stats::model.matrix(~ f, data = data.frame(f = f), na.action = stats::na.pass)
    if (nrow(mm_full) != length(f)) stop(sprintf("expand_factors_to_numeric(): factor '%s' produced %d rows but expected %d.", fn, nrow(mm_full), length(f)), call. = FALSE)
    mm <- mm_full[, -1L, drop = FALSE]
    colnames(mm) <- paste0(fn, "_", levs[-1L])
    dummy_parts[[fn]] <- mm
  }
  if (length(dummy_parts) == 0L) return(num_part)
  # v6: ensure data.frame return type. cbind(data.frame, matrix) can return
  # a matrix under R's method dispatch, which crashes earth's formula path.
  out <- cbind(num_part, do.call(cbind, dummy_parts))
  if (!is.data.frame(out)) out <- as.data.frame(out, stringsAsFactors = FALSE)
  out
}

compute_earnings <- function(h4ec2, h4ec3, cfg_outcome) {
  if (length(h4ec2) != length(h4ec3))
    stop("Exact and bracket earnings variables must have the same length.", call. = FALSE)
  exact   <- suppressWarnings(as.numeric(as.character(h4ec2)))
  bracket <- suppressWarnings(as.numeric(as.character(h4ec3)))

  valid_exact <- is.finite(exact) &
    exact >= (cfg_outcome$exact_valid_min %||% 0) &
    exact <= (cfg_outcome$exact_valid_max %||% 999995)
  known_exact_missing <- is.na(exact) | exact %in% (cfg_outcome$exact_missing_codes %||% numeric(0))
  unexpected_exact <- sort(unique(exact[is.finite(exact) & !valid_exact & !known_exact_missing]))
  if (length(unexpected_exact) > 0L) {
    stop("Unexpected finite exact-earnings code(s): ",
         paste(unexpected_exact, collapse = ", "), call. = FALSE)
  }

  valid_bracket_codes <- as.numeric(cfg_outcome$bracket_valid_codes %||% names(cfg_outcome$bracket_map))
  valid_bracket <- is.finite(bracket) & bracket %in% valid_bracket_codes
  known_bracket_missing <- is.na(bracket) |
    bracket %in% (cfg_outcome$bracket_missing_codes %||% numeric(0))
  unexpected_bracket <- sort(unique(bracket[is.finite(bracket) & !valid_bracket & !known_bracket_missing]))
  if (length(unexpected_bracket) > 0L) {
    stop("Unexpected finite bracket-earnings code(s): ",
         paste(unexpected_bracket, collapse = ", "), call. = FALSE)
  }

  out <- rep(NA_real_, length(exact))
  source <- rep("missing_both", length(exact))
  out[valid_exact] <- exact[valid_exact]
  source[valid_exact] <- "exact"
  use_bracket <- !valid_exact & valid_bracket
  out[use_bracket] <- unname(cfg_outcome$bracket_map[as.character(bracket[use_bracket])])
  source[use_bracket] <- "bracket_midpoint"
  source[!valid_exact & !valid_bracket & known_exact_missing] <- "exact_missing_no_valid_bracket"

  audit <- data.frame(
    metric = c("n_total", "n_exact_valid", "n_zero_exact", "n_exact_missing_code",
               "n_bracket_substituted", "n_missing_after_both", "n_top_open_bracket"),
    value = c(length(out), sum(valid_exact), sum(valid_exact & exact == 0),
              sum(is.finite(exact) & exact %in% (cfg_outcome$exact_missing_codes %||% numeric(0))),
              sum(use_bracket), sum(!is.finite(out)),
              sum(use_bracket & bracket == max(valid_bracket_codes, na.rm = TRUE))),
    stringsAsFactors = FALSE)
  bracket_counts <- as.data.frame(table(factor(bracket[use_bracket], levels = valid_bracket_codes)))
  names(bracket_counts) <- c("bracket_code", "value")
  bracket_counts$metric <- paste0("n_bracket_code_", bracket_counts$bracket_code)
  audit <- rbind(audit, bracket_counts[, c("metric", "value")])
  list(earnings = out, source = source, audit = audit,
       exact = exact, bracket = bracket)
}

detect_binary01 <- function(x) {
  missing <- character_native_missing_mask(x)
  if (all(missing)) return(FALSE)
  if (is.logical(x)) return(TRUE)
  z <- to_num(x)
  if (any(!missing & !is.finite(z))) return(FALSE)
  ux <- sort(unique(as.numeric(z[!missing & is.finite(z)])))
  identical(ux, c(0, 1))
}

infer_variable_type <- function(x, requested = "auto", name = "variable") {
  if (!requested %in% c("auto","binary","continuous"))
    stop("Requested type for ", name, " must be 'auto', 'binary', or 'continuous'.", call. = FALSE)
  if (requested != "auto") return(requested)
  if (detect_binary01(x)) "binary" else "continuous"
}

normalize_binary_var <- function(x, name = "variable", require_both = TRUE) {
  missing <- character_native_missing_mask(x)
  if (any(missing))
    stop(name, " contains missing or blank values; binary variable must be fully observed here.", call. = FALSE)
  z <- if (is.logical(x)) as.integer(x) else to_num(x)
  if (any(!is.finite(z)))
    stop(name, " must be explicitly coded numeric 0/1; arbitrary text-level ordering is not allowed.", call. = FALSE)
  ux <- sort(unique(as.numeric(z)))
  if (!length(ux) || any(!ux %in% c(0, 1)) ||
      (isTRUE(require_both) && !identical(ux, c(0, 1)))) {
    requirement <- if (isTRUE(require_both)) "contain both explicitly coded values 0 and 1" else
      "contain only explicitly coded values 0 and/or 1"
    stop(name, " must ", requirement, ".", call. = FALSE)
  }
  as.integer(z)
}

normalize_binary_allow_missing <- function(x, name = "variable") {
  missing <- character_native_missing_mask(x)
  out <- rep(NA_integer_, length(x))
  keep <- !missing
  if (!any(keep)) return(out)
  # Binary outcomes and observation indicators may legitimately be constant in
  # the full data (for example, a completely observed outcome). Coding validity
  # is separate from the later learner-specific requirement for both classes.
  out[keep] <- normalize_binary_var(x[keep], name, require_both = FALSE)
  out
}

make_observed_mask <- function(x) {
  if (is.numeric(x) || is.integer(x)) is.finite(x) else !character_native_missing_mask(x)
}

prepare_modeled_outcome <- function(x, requested = "auto", name = "outcome") {
  type <- infer_variable_type(x, requested, name)
  if (identical(type, "binary")) {
    values <- normalize_binary_allow_missing(x, name)
  } else {
    missing <- character_native_missing_mask(x)
    values <- to_num(x)
    bad <- !missing & !is.finite(values)
    if (any(bad)) {
      examples <- unique(trimws(as.character(x[bad])))
      examples <- examples[seq_len(min(length(examples), 5L))]
      stop(sprintf("%s contains %d observed nonnumeric value(s) (examples: %s).",
                   name, sum(bad), paste(examples, collapse = ", ")), call. = FALSE)
    }
    values[missing | !is.finite(values)] <- NA_real_
  }
  list(values = values, type = type, observed = make_observed_mask(values))
}

# Identify the "knee" in a ranked score curve - the point of diminishing
# returns, used as the rough-prescreen cutoff.
make_knee_cutoff <- function(scores) {
  s <- sort(scores, decreasing = TRUE, na.last = NA)
  if (length(s) == 0L) stop("Cannot compute knee cutoff from empty score vector.", call. = FALSE)
  if (length(s) == 1L || isTRUE(all.equal(max(s), min(s))))
    return(list(sorted_scores = s, rank = 1L, value = s[1L]))
  x  <- seq_along(s)
  xn <- (x - x[1]) / (x[length(x)] - x[1])
  yn <- (s - s[length(s)]) / (s[1] - s[length(s)])
  d  <- abs(yn - (1 - xn)) / sqrt(2)
  knee_rank <- which.max(d)
  list(sorted_scores = s, rank = knee_rank, value = s[knee_rank])
}

plot_knee_curve <- function(knee_obj, label_y = "Score") {
  s <- knee_obj$sorted_scores
  plot(s, type = "l", xlab = "Rank best -> worst", ylab = label_y)
  text(knee_obj$rank, knee_obj$value + 0.02,
       labels = sprintf("Cutoff = %.4f (rank %d)", knee_obj$value, knee_obj$rank),
       pos = 4, xpd = NA)
}

# v5.1: Harmonize cutoff_rule and the legacy use_knee_cutoff toggle.
# Precedence: explicit cutoff_rule wins; otherwise fall back to legacy toggle.
resolve_cutoff_rule <- function(rp_cfg) {
  if (!is.null(rp_cfg$cutoff_rule)) return(rp_cfg$cutoff_rule)
  if (isTRUE(rp_cfg$use_knee_cutoff))  return("knee")
  if (identical(rp_cfg$use_knee_cutoff, FALSE)) {
    warning("use_knee_cutoff = FALSE is ambiguous; defaulting to cutoff_rule = 'positive'. ",
            "Set cfg$rough_prescreen$cutoff_rule explicitly to silence this.")
    return("positive")
  }
  "knee"
}

# v5.1: Unified cutoff application for the rough prescreen.
# "positive" keeps variables that beat the null model (score > min_score).
# "knee" uses the geometric knee on the sorted-score curve.
# "topk" keeps the top max_keep variables by score.
# max_keep is applied as a safety cap regardless of rule.
apply_cutoff <- function(scores, knee, rule, min_score = 0, max_keep = 2000L) {
  s <- sort(scores[is.finite(scores)], decreasing = TRUE)
  kept <- switch(rule,
    "knee"     = s[s >= knee$value],
    "positive" = s[s >  min_score],
    "topk"     = s[seq_len(min(length(s), max_keep))],
    s[s >= knee$value])
  if (length(kept) > max_keep) kept <- kept[seq_len(max_keep)]
  kept
}


# =============================================================================
