# =============================================================================
# 12b) MULTI-SEED ATT ALGORITHMIC-STABILITY DIAGNOSTIC
# =============================================================================
# Runs the complete pipeline under a fixed seed set and reports the spread of
# estimates and model-selection behavior. The designated pipeline seed remains
# the headline analysis; repeated seeds are not independent datasets and are
# not pooled into a new inferential confidence interval.
extract_multiseed_selection_sets <- function(pipeline_result, seed) {
  if (is.null(pipeline_result$results) || !length(pipeline_result$results))
    return(list())
  out <- list()
  for (tag in names(pipeline_result$results)) {
    one <- pipeline_result$results[[tag]]
    sbf <- one$tmle_fit$selected_by_fold %||% list()
    if (!length(sbf)) next
    union_vars <- sort(unique(unlist(sbf, use.names = FALSE)))
    core_vars <- union_vars[vapply(union_vars, function(v)
      all(vapply(sbf, function(z) v %in% z, logical(1))), logical(1))]
    seed_suffix <- paste0("__seed", as.integer(seed))
    analysis_tag <- if (endsWith(tag, seed_suffix))
      substr(tag, 1L, nchar(tag) - nchar(seed_suffix)) else tag
    out[[analysis_tag]] <- list(
      seed = as.integer(seed),
      run_tag = analysis_tag,
      seed_specific_run_tag = tag,
      union = union_vars,
      core = sort(core_vars))
  }
  out
}

write_multiseed_selection_stability <- function(selection_sets, out_dir, cfg) {
  flat <- do.call(c, unname(selection_sets))
  if (!length(flat)) return(invisible(NULL))
  per_seed <- do.call(rbind, lapply(flat, function(z) data.frame(
    seed = z$seed, run_tag = z$run_tag,
    seed_specific_run_tag = z$seed_specific_run_tag %||% NA_character_,
    n_selected_union = length(z$union), n_selected_core = length(z$core),
    union_fingerprint_md5 = object_md5(z$union),
    core_fingerprint_md5 = object_md5(z$core),
    stringsAsFactors = FALSE)))
  write_provenance_csv_at_path(per_seed, cfg,
    file.path(out_dir, "ms_sel_seed.csv"),
    "ms_sel_seed.csv", overwrite = TRUE)

  tags <- unique(vapply(flat, `[[`, character(1), "run_tag"))
  freq_rows <- list(); jac_rows <- list()
  for (tag in tags) {
    zz <- flat[vapply(flat, function(z) identical(z$run_tag, tag), logical(1))]
    all_vars <- sort(unique(unlist(lapply(zz, `[[`, "union"), use.names = FALSE)))
    if (length(all_vars)) {
      freq_rows[[tag]] <- data.frame(
        run_tag = tag, variable = all_vars,
        n_seeds_union = vapply(all_vars, function(v)
          sum(vapply(zz, function(z) v %in% z$union, logical(1))), integer(1)),
        n_seeds_core = vapply(all_vars, function(v)
          sum(vapply(zz, function(z) v %in% z$core, logical(1))), integer(1)),
        n_seeds = length(zz), stringsAsFactors = FALSE)
      freq_rows[[tag]]$fraction_seeds_union <-
        freq_rows[[tag]]$n_seeds_union / freq_rows[[tag]]$n_seeds
      freq_rows[[tag]]$fraction_seeds_core <-
        freq_rows[[tag]]$n_seeds_core / freq_rows[[tag]]$n_seeds
    }
    if (length(zz) > 1L) {
      pairs <- utils::combn(seq_along(zz), 2L)
      jac_rows[[tag]] <- do.call(rbind, lapply(seq_len(ncol(pairs)), function(j) {
        a <- zz[[pairs[1L, j]]]; b <- zz[[pairs[2L, j]]]
        u <- union(a$union, b$union); ii <- intersect(a$union, b$union)
        data.frame(run_tag = tag, seed_1 = a$seed, seed_2 = b$seed,
                   n_union_seed_1 = length(a$union), n_union_seed_2 = length(b$union),
                   n_intersection = length(ii), n_combined_union = length(u),
                   jaccard_union = if (length(u)) length(ii) / length(u) else NA_real_,
                   stringsAsFactors = FALSE)
      }))
    }
  }
  if (length(freq_rows)) write_provenance_csv_at_path(
    do.call(rbind, freq_rows), cfg,
    file.path(out_dir, "ms_sel_freq.csv"),
    "ms_sel_freq.csv", overwrite = TRUE)
  if (length(jac_rows)) write_provenance_csv_at_path(
    do.call(rbind, jac_rows), cfg,
    file.path(out_dir, "ms_sel_jac.csv"),
    "ms_sel_jac.csv", overwrite = TRUE)
  invisible(per_seed)
}

run_multiseed_att <- function(base_cfg,
                              seeds = NULL,
                              out_dir = NULL,
                              fresh = FALSE) {
  base_cfg <- apply_outcome_runtime_defaults(base_cfg)
  base_cfg <- ensure_run_id(base_cfg)
  validate_cfg(base_cfg)
  load_required_packages(base_cfg)
  if (isTRUE(base_cfg$stages$run_preflight_unit_test))
    run_preflight_unit_test(base_cfg)
  base_cfg$stages$run_preflight_unit_test <- FALSE
  # Multi-seed runs must be able to rebuild preprocessing artifacts when a
  # a version or source fingerprint makes a cache incompatible. Prefer compatible
  # caches, but never make a harmless cache miss fatal because the caller had
  # disabled construction stages.
  base_cfg$stages$run_read_wave1_phase <- TRUE
  base_cfg$stages$run_build_main_dataset_phase <- TRUE
  base_cfg$cache$use_cached_wave1 <- TRUE
  base_cfg$cache$use_cached_main_dataset <- TRUE
  if (is.null(seeds) || !length(seeds))
    stop("run_multiseed_att: no seeds supplied; set cfg$global$multiseed_seeds.", call. = FALSE)
  seeds <- as.integer(seeds)
  if (anyNA(seeds) || anyDuplicated(seeds))
    stop("run_multiseed_att: seeds must be unique finite integers.", call. = FALSE)
  out_dir <- out_dir %||% base_cfg$global$output_dir
  ensure_output_dir(out_dir, cfg = base_cfg, test_write = TRUE)
  message(sprintf("\n===== STAGE: Multi-seed ATT algorithmic stability (%d seeds) =====", length(seeds)))
  per_seed_csv <- file.path(out_dir, "ms_seed.csv")
  agg_csv <- file.path(out_dir, "ms_sum.csv")
  checkpoint <- file.path(out_dir, "ms_ck.rds")
  selection_paths <- file.path(out_dir, c(
    "ms_sel_seed.csv",
    "ms_sel_freq.csv",
    "ms_sel_jac.csv"))

  aggregate_cfg <- base_cfg
  aggregate_cfg$global$output_dir <- out_dir
  aggregate_cfg$global$run_label <- "multiseed"
  aggregate_cfg$global$resume_mode <- FALSE
  aggregate_cfg <- freeze_run_provenance(aggregate_cfg)
  ms_sig <- list(
    seeds = sort(seeds), version = base_cfg$global$version %||% "NA",
    script = get_frozen_source_fingerprint(aggregate_cfg),
    analysis = base_cfg$analysis, exposure = base_cfg$exposure,
    outcome = base_cfg$outcome, preprocessing = base_cfg$preprocessing,
    final_preprocess = base_cfg$final_preprocess, final_tmle = base_cfg$final_tmle,
    learners = base_cfg$learners, causal_governance = base_cfg$causal_governance,
    mortality_sensitivity = base_cfg$mortality_sensitivity,
    policy = base_cfg$policy, diagnostics = base_cfg$diagnostics,
    safety = base_cfg$safety,
    source_files = lapply(base_cfg$paths, file_fingerprint))

  collected <- list(); selection_collected <- list(); resume_ok <- FALSE
  if (!isTRUE(fresh) && file.exists(checkpoint)) {
    ck <- tryCatch(readRDS(checkpoint), error = function(e) NULL)
    if (is.list(ck) && !is.null(ck$sig) && identical(ck$sig, ms_sig)) {
      collected <- ck$data %||% list()
      selection_collected <- ck$selection %||% list()
      prior_run_id <- as.character(ck$aggregate_run_id %||% "")
      if (!nzchar(prior_run_id))
        stop("Compatible multiseed checkpoint lacks aggregate_run_id; use fresh=TRUE in a new output directory.",
             call. = FALSE)
      aggregate_cfg$global$run_id <- prior_run_id
      aggregate_cfg$global$resume_mode <- TRUE
      aggregate_cfg <- freeze_run_provenance(aggregate_cfg)
      resume_ok <- TRUE
      message(sprintf("  [multiseed] resuming aggregate run %s with %d completed seed(s).",
                      prior_run_id, length(collected)))
    } else {
      message("  [multiseed] checkpoint fingerprint changed (or unreadable); a fresh directory is required.")
    }
  }

  known_aggregate_paths <- c(per_seed_csv, agg_csv, checkpoint, selection_paths)
  known_seed_dirs <- file.path(out_dir, paste0("seed_", seeds))
  if (!resume_ok) {
    archived_fresh <- character(0)
    if (isTRUE(fresh)) {
      for (q in c(known_aggregate_paths, known_seed_dirs)) {
        if (file.exists(q) || dir.exists(q))
          archived_fresh <- c(archived_fresh,
                              archive_existing_path(q, "FRESH"))
      }
    }
    # Files archived by fresh=TRUE receive explicit names inside the multiseed
    # root. Permit those names and the normal run-log allowlist while rejecting
    # every other unexpected entry.
    allowed_fresh <- unique(c(
      base_cfg$safety$fresh_output_allowed_basenames %||% character(0),
      basename(archived_fresh)))
    assert_fresh_output_dir(out_dir, base_cfg,
                            allowed_basenames = allowed_fresh)
    aggregate_cfg$global$resume_mode <- FALSE
    aggregate_cfg <- freeze_run_provenance(aggregate_cfg)
  }

  write_checkpoint <- function() atomic_save_rds(
    list(sig = ms_sig, aggregate_run_id = aggregate_cfg$global$run_id,
         data = collected, selection = selection_collected),
    checkpoint, overwrite = TRUE)
  write_per_seed <- function() {
    if (!length(collected)) return(invisible(NULL))
    schemas <- lapply(collected, names)
    if (!all(vapply(schemas, identical, logical(1), schemas[[1L]])))
      stop("Multi-seed result-schema drift detected; do not combine incompatible runs.", call. = FALSE)
    write_provenance_csv_at_path(do.call(rbind, collected), aggregate_cfg,
      per_seed_csv, "ms_seed.csv", overwrite = TRUE)
  }

  par_cores <- suppressWarnings(as.integer(base_cfg$global$multiseed_parallel_cores %||% 1L))
  if (!is.finite(par_cores) || is.na(par_cores)) par_cores <- 1L
  par_cores <- max(1L, par_cores)
  if (par_cores > 1L && .Platform$OS.type == "windows") {
    message("  [multiseed] Forked mclapply is unavailable on Windows; running sequentially.")
    par_cores <- 1L
  }
  seeds_run <- seeds[!as.character(seeds) %in% names(collected)]
  if (par_cores > 1L && length(seeds_run) > 1L) {
    n_work <- min(par_cores, length(seeds_run))
    par_res <- parallel::mclapply(seeds_run, function(s) {
      key <- as.character(s); cfg_s <- base_cfg
      cfg_s$global$pipeline_seed <- as.integer(s)
      cfg_s$global$run_label <- paste0("seed", key)
      cfg_s$global$run_id <- NULL
      cfg_s$global$resume_mode <- FALSE
      cfg_s$global$output_dir <- file.path(out_dir, paste0("seed_", key))
      if (dir.exists(cfg_s$global$output_dir))
        archive_existing_path(cfg_s$global$output_dir, "INCOMPLETE")
      cfg_s$cache$use_cached_wave1 <- FALSE
      cfg_s$cache$use_cached_main_dataset <- FALSE
      r <- tryCatch(run_addhealth_pipeline(cfg_s), error = function(e) {
        if (dir.exists(cfg_s$global$output_dir))
          try(archive_existing_path(cfg_s$global$output_dir, "FAILED"), silent = TRUE)
        structure(list(error = conditionMessage(e)), class = "multiseed_failure")
      })
      if (inherits(r, "multiseed_failure")) return(r)
      if (is.null(r$summary) || !nrow(r$summary))
        return(structure(list(error = "No successful summary rows returned."),
                         class = "multiseed_failure"))
      rows <- r$summary; rows$seed <- as.integer(s)
      list(rows = rows, selection = extract_multiseed_selection_sets(r, s),
           error = NA_character_)
    }, mc.cores = n_work, mc.preschedule = FALSE)
    for (i in seq_along(seeds_run)) {
      one <- par_res[[i]]
      if (inherits(one, "multiseed_failure") || is.null(one$rows)) {
        message(sprintf("  [multiseed] seed %s FAILED: %s",
                        seeds_run[i], one$error %||% "Unknown parallel failure.")); next
      }
      key <- as.character(seeds_run[i])
      collected[[key]] <- one$rows
      selection_collected[[key]] <- one$selection %||% list()
    }
    write_checkpoint(); write_per_seed()
  } else {
    for (s in seeds) {
      key <- as.character(s)
      if (!is.null(collected[[key]])) {
        message(sprintf("  [multiseed] seed %s already done; skipping.", key)); next
      }
      cfg_s <- base_cfg
      cfg_s$global$pipeline_seed <- as.integer(s)
      cfg_s$global$run_label <- paste0("seed", key)
      cfg_s$global$run_id <- NULL
      cfg_s$global$resume_mode <- FALSE
      cfg_s$global$output_dir <- file.path(out_dir, paste0("seed_", key))
      if (dir.exists(cfg_s$global$output_dir))
        archive_existing_path(cfg_s$global$output_dir, "INCOMPLETE")
      cfg_s$cache$use_cached_wave1 <- TRUE
      cfg_s$cache$use_cached_main_dataset <- TRUE
      r <- tryCatch(run_addhealth_pipeline(cfg_s), error = function(e) {
        message(sprintf("  [multiseed] seed %s FAILED: %s", key, conditionMessage(e)))
        if (dir.exists(cfg_s$global$output_dir))
          try(archive_existing_path(cfg_s$global$output_dir, "FAILED"), silent = TRUE)
        NULL
      })
      if (is.null(r) || is.null(r$summary) || !nrow(r$summary)) next
      rows <- r$summary; rows$seed <- as.integer(s)
      collected[[key]] <- rows
      selection_collected[[key]] <- extract_multiseed_selection_sets(r, s)
      write_checkpoint(); write_per_seed()
      message(sprintf("  [multiseed] seed %s complete; wrote %d row(s).", key, nrow(rows)))
    }
  }

  if (isTRUE(base_cfg$global$multiseed_require_all_seeds %||% FALSE) &&
      length(collected) < length(seeds))
    stop(sprintf("run_multiseed_att: only %d of %d seeds succeeded; all are required.",
                 length(collected), length(seeds)), call. = FALSE)
  if (!length(collected)) {
    message("  [multiseed] no successful seeds."); return(invisible(NULL))
  }
  schemas <- lapply(collected, names)
  if (!all(vapply(schemas, identical, logical(1), schemas[[1L]])))
    stop("Multi-seed result-schema drift detected; do not combine incompatible runs.", call. = FALSE)
  all_rows <- do.call(rbind, collected)
  if (!all(c("att_estimate", "att_se") %in% names(all_rows)))
    return(invisible(list(per_seed = all_rows, aggregate = NULL)))

  grp_cols <- intersect(c("family", "wave", "family_member", "primary_estimand", "estimand",
                          "compensation_transform", "compensation_exact_only", "cap_probability"),
                        names(all_rows))
  if (length(grp_cols)) {
    all_missing <- vapply(all_rows[grp_cols], function(z) {
      zz <- trimws(as.character(z)); all(is.na(z) | is.na(zz) | !nzchar(zz))
    }, logical(1))
    grp_cols <- grp_cols[!all_missing]
  }
  if (length(grp_cols)) {
    group_frame <- all_rows[grp_cols]
    group_frame[] <- lapply(group_frame, function(z) {
      zz <- as.character(z); zz[is.na(zz) | !nzchar(trimws(zz))] <- "<NA>"; zz
    })
    split_key <- do.call(interaction, c(as.list(group_frame),
                                        list(drop = TRUE, lex.order = TRUE)))
  } else split_key <- factor(rep("all", nrow(all_rows)))
  designated_seed <- as.integer(base_cfg$global$pipeline_seed)
  agg <- do.call(rbind, lapply(split(all_rows, split_key), function(d) {
    d <- d[is.finite(d$att_estimate) & is.finite(d$att_se), , drop = FALSE]
    if (!nrow(d)) return(NULL)
    est <- d$att_estimate; se <- d$att_se
    designated <- d[d$seed == designated_seed, , drop = FALSE]
    hdr <- if (length(grp_cols)) d[1L, grp_cols, drop = FALSE] else data.frame(group = "all")
    cbind(hdr, data.frame(
      n_seeds = nrow(d), designated_seed = designated_seed,
      designated_seed_estimate = if (nrow(designated) == 1L) designated$att_estimate else NA_real_,
      designated_seed_se = if (nrow(designated) == 1L) designated$att_se else NA_real_,
      att_median_across_seeds = stats::median(est), att_mean_across_seeds = mean(est),
      att_sd_across_seeds = if (length(est) > 1L) stats::sd(est) else 0,
      att_min_across_seeds = min(est), att_max_across_seeds = max(est),
      att_range_across_seeds = max(est) - min(est),
      max_abs_deviation_from_designated = if (nrow(designated) == 1L)
        max(abs(est - designated$att_estimate)) else NA_real_,
      median_within_seed_se = stats::median(se),
      seeds = paste(sort(d$seed), collapse = ";"),
      inference_status = "descriptive_algorithmic_stability_only_no_pooled_ci",
      stringsAsFactors = FALSE))
  }))
  rownames(agg) <- NULL
  write_provenance_csv_at_path(agg, aggregate_cfg, agg_csv,
                               "ms_sum.csv", overwrite = TRUE)
  write_multiseed_selection_stability(selection_collected, out_dir, aggregate_cfg)
  verify_frozen_source_unchanged(aggregate_cfg)
  message(sprintf("  [multiseed] wrote per-seed (%s), aggregate (%s), and selection-stability artifacts.",
                  basename(per_seed_csv), basename(agg_csv)))
  message("  [multiseed] Aggregate is descriptive only; the designated seed remains the inferential headline.")
  invisible(list(per_seed = all_rows, aggregate = agg))
}


# =============================================================================
