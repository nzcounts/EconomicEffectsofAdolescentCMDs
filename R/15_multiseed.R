# =============================================================================
# 12b) OPTIONAL REPEATED-SPLIT ATT AGGREGATION AND STABILITY
# =============================================================================
# Runs the full algorithm under each fixed seed; only this explicitly enabled
# runner reports an aggregated ATT. The usual single-run entry point is unchanged.
# Repeated estimates use the SAME data, not independent replicates.
# Repeated-split aggregation on the ESTIMATOR-variance scale. The input att_se
# is already the survey SE of an estimate, not the SD of one EIF observation.
# References: Chernozhukov et al. (2018), Eq. 3.14,
# https://doi.org/10.1111/ectj.12097 ; Zivich & Breskin (2021),
# https://pmc.ncbi.nlm.nih.gov/articles/PMC8012235/ .
# This adopts that aggregation with each split's survey-design variance. The
# common survey t reference is retained; it is not a new finite-sample coverage
# theorem for this custom sequential survey ATT.
validate_multiseed_ids <- function(seeds,label="seeds") {
  if (!is.numeric(seeds) || !length(seeds) || any(!is.finite(seeds)) ||
      any(seeds < 0 | seeds > .Machine$integer.max) ||
      any(seeds != trunc(seeds)) || anyDuplicated(seeds))
    stop(label," must be unique nonnegative integer-valued seeds.",call.=FALSE)
  as.integer(seeds)
}

# Remove the seed suffix when grouping repeated-split outcomes.
# Accept supported underscore separators and reject absent or wrong suffixes.

multiseed_analysis_tag <- function(tag,seed) {
  seed <- validate_multiseed_ids(seed,"Tag seed")
  if (length(seed)!=1L || !is.character(tag) || !length(tag) || anyNA(tag) ||
      any(!nzchar(tag))) stop("Invalid multi-seed analysis tag.",call.=FALSE)
  vapply(tag,function(z) {
    suffixes <- c(paste0("__seed",seed),paste0("_seed",seed))
    hit <- which(vapply(suffixes,function(a) endsWith(z,a),logical(1)))
    if (!length(hit)) stop("Run tag does not end in its exact seed label: ",z,call.=FALSE)
    ans <- substr(z,1L,nchar(z)-nchar(suffixes[hit[1L]]))
    if (!nzchar(ans)) stop("Empty multi-seed analysis tag.",call.=FALSE)
    ans
  },character(1),USE.NAMES=FALSE)
}

# Retain only aggregate fingerprints outside the restricted workspace. The
# fingerprint checks exact persons, design, exposure, observation and outcome
# construction across seeds, NOT fitted nuisances, fold allocation or EIF values.
prepare_multiseed_att_rows <- function(pipeline_result,seed,cfg,signature) {
  seed <- validate_multiseed_ids(seed,"Per-run seed")
  if (length(seed)!=1L) stop("Expected one per-run seed.",call.=FALSE)
  rows <- pipeline_result$summary
  runs <- pipeline_result$results
  if (!is.data.frame(rows) || !nrow(rows) || !"run_tag" %in% names(rows) ||
      anyNA(rows$run_tag) || anyDuplicated(rows$run_tag) || !is.list(runs))
    stop("Multi-seed run has incomplete or duplicated outcome summaries.",call.=FALSE)
  rows$seed <- seed
  rows$multisplit_analysis_tag <- multiseed_analysis_tag(as.character(rows$run_tag),seed)
  rows$multisplit_spec_md5 <- object_md5(signature)
  rows$multisplit_source_md5 <- NA_character_
  rows$multisplit_sample_md5 <- NA_character_
  rows$att_inference_df <- NA_real_
  rows$att_inference_method <- NA_character_
  for (i in seq_len(nrow(rows))) {
    one <- runs[[as.character(rows$run_tag[i])]]
    f <- one$tmle_fit
    ac <- f$att_components
    rr <- f$result
    if (is.null(f) || is.null(ac) || !is.data.frame(rr) || nrow(rr)!=1L ||
        !is.finite(ac$psi_att) || !is.finite(ac$att_se) || ac$att_se<=0 ||
        !isTRUE(all.equal(as.numeric(rows$att_estimate[i]),as.numeric(ac$psi_att),tolerance=1e-12)) ||
        !isTRUE(all.equal(as.numeric(rows$att_se[i]),as.numeric(ac$att_se),tolerance=1e-12)))
      stop("Multi-seed summary does not reconcile with its ATT fit.",call.=FALSE)
    if (length(rr$script_md5)!=1L || is.na(rr$script_md5) ||
        !identical(as.character(rr$script_md5),as.character(signature$script$md5)))
      stop("Multi-seed source fingerprint differs from the fixed source.",call.=FALSE)
    ids <- as.character(f$respondent_ids)
    n <- length(ids)
    if (!n || anyNA(ids) || anyDuplicated(ids) || any(!nzchar(ids)))
      stop("Invalid multi-seed participant identifiers.",call.=FALSE)
    parts <- list(A=ac$A,Y_raw=ac$Y_raw,Y_bounded=ac$Y_bounded_orig,
      delta_Y=ac$delta_Y,weights=ac$weights,cluster=ac$cluster,strata=ac$strata,
      eligibility=f$eligibility,structural_zero=f$structural_zero)
    if (any(vapply(parts,length,integer(1))!=n))
      stop("Multi-seed sample fingerprint fields do not align.",call.=FALSE)
    ord <- order(ids)
    frame <- f$survey_design_frame
    frame_fields <- c(cfg$analysis$id_var,cfg$analysis$cluster_var,
      cfg$analysis$strata_var,cfg$analysis$weight_var,".analysis_domain")
    if (!is.data.frame(frame) || !all(frame_fields %in% names(frame)))
      stop("Multi-seed fit lacks its full survey design frame.",call.=FALSE)
    frame_id <- as.character(frame[[cfg$analysis$id_var]])
    if (anyNA(frame_id) || anyDuplicated(frame_id) || any(!nzchar(frame_id)))
      stop("Invalid full-frame IDs for multi-seed compatibility.",call.=FALSE)
    # Hash semantic design values, not cache metadata, row names or other
    # data-frame attributes that can differ without changing the estimand.
    frame_order <- order(frame_id)
    frame <- list(id=frame_id[frame_order],
      cluster=as.character(frame[[cfg$analysis$cluster_var]])[frame_order],
      strata=as.character(frame[[cfg$analysis$strata_var]])[frame_order],
      weights=as.numeric(frame[[cfg$analysis$weight_var]])[frame_order],
      domain=as.logical(frame$.analysis_domain)[frame_order])
    fingerprint <- list(ids=ids[ord],sample=lapply(parts,function(z) unname(z[ord])),
      full_design=frame,lower=f$y_lower,upper=f$y_upper,outcome_type=f$outcome_type,
      composite_scope=f$composite_scope,target_population=configured_target_population(cfg))
    rows$multisplit_sample_md5[i] <- object_md5(fingerprint)
    rows$multisplit_source_md5[i] <- as.character(rr$script_md5)
    rows$att_inference_df[i] <- as.numeric(ac$inference_df)
    rows$att_inference_method[i] <- as.character(ac$inference_method)
  }
  rows
}

aggregate_multiseed_att <- function(all_rows,seeds,designated_seed) {
  seeds <- validate_multiseed_ids(seeds)
  designated_seed <- validate_multiseed_ids(designated_seed,"Designated seed")
  if (length(designated_seed)!=1L) stop("Expected one designated seed.",call.=FALSE)
  required <- c("seed","att_estimate","att_se","att_inference_df","att_inference_method",
    "multisplit_analysis_tag","multisplit_spec_md5","multisplit_source_md5","multisplit_sample_md5")
  if (!is.data.frame(all_rows) || !nrow(all_rows) || !all(required %in% names(all_rows)))
    stop("Incomplete multi-seed aggregation input.",call.=FALSE)
  if (!is.numeric(all_rows$seed) || any(!is.finite(all_rows$seed)) ||
      any(all_rows$seed!=trunc(all_rows$seed)) ||
      !setequal(as.integer(all_rows$seed),seeds))
    stop("All fixed seeds, and no additional seeds, are required for aggregation.",call.=FALSE)
  numeric_fields <- c("att_estimate","att_se","att_inference_df")
  if (any(!vapply(all_rows[numeric_fields],is.numeric,logical(1))) ||
      any(!is.finite(as.matrix(all_rows[numeric_fields]))) ||
      any(all_rows$att_se<=0 | all_rows$att_inference_df<=0))
    stop("Every split needs a finite ATT, positive SE and positive survey df.",call.=FALSE)
  text_fields <- setdiff(required,c("seed",numeric_fields))
  for (nm in text_fields) {
    z <- as.character(all_rows[[nm]])
    if (anyNA(z) || any(!nzchar(trimws(z))))
      stop("Missing multi-seed compatibility field: ",nm,call.=FALSE)
  }
  for (nm in c("multisplit_spec_md5","multisplit_source_md5"))
    if (length(unique(as.character(all_rows[[nm]])))!=1L)
      stop("Cannot aggregate changed source/specification: ",nm,call.=FALSE)

  grp_cols <- unique(c("multisplit_analysis_tag",intersect(c(
    "family","wave","family_member","primary_estimand","estimand",
    "compensation_transform","compensation_exact_only","cap_probability"),names(all_rows))))
  # Hash the actual typed group values instead of ambiguous pasted labels.
  keys <- vapply(seq_len(nrow(all_rows)),function(i)
    object_md5(lapply(all_rows[i,grp_cols,drop=FALSE],function(z) unname(z))),character(1))
  groups <- split(seq_len(nrow(all_rows)),keys)
  ans <- lapply(groups,function(ii) {
    d <- all_rows[ii,,drop=FALSE]
    if (nrow(d)!=length(seeds) || anyDuplicated(d$seed) || !setequal(as.integer(d$seed),seeds))
      stop("Each outcome/subgroup needs exactly one successful row from EVERY fixed seed.",call.=FALSE)
    for (nm in c("multisplit_sample_md5","att_inference_method","att_inference_df"))
      if (length(unique(d[[nm]]))!=1L)
        stop("Split estimates differ in sample/target/design inference: ",nm,call.=FALSE)
    est <- d$att_estimate; se <- d$att_se
    med <- stats::median(est)
    split_variances <- se^2 + (est-med)^2
    variance <- stats::median(split_variances)
    if (!is.finite(variance) || variance<=0)
      stop("Invalid multi-split estimator variance.",call.=FALSE)
    pooled_se <- sqrt(variance)
    df <- as.numeric(d$att_inference_df[1L])
    crit <- stats::qt(.975,df=df)
    designated <- which(d$seed==designated_seed)
    hdr <- d[1L,grp_cols,drop=FALSE]
    cbind(hdr,data.frame(
      n_seeds=nrow(d),designated_seed=designated_seed,
      designated_seed_estimate=if(length(designated)) est[designated] else NA_real_,
      designated_seed_se=if(length(designated)) se[designated] else NA_real_,
      att_median_across_seeds=med,att_mean_across_seeds=mean(est),
      att_sd_across_seeds=if(length(est)>1L) stats::sd(est) else 0,
      att_min_across_seeds=min(est),att_max_across_seeds=max(est),
      att_range_across_seeds=max(est)-min(est),
      max_abs_deviation_from_designated=if(length(designated)) max(abs(est-est[designated])) else NA_real_,
      median_within_seed_se=stats::median(se),
      att_multisplit_estimate=med,att_multisplit_variance=variance,
      att_multisplit_se=pooled_se,
      att_multisplit_ci_lower=med-crit*pooled_se,
      att_multisplit_ci_upper=med+crit*pooled_se,
      att_multisplit_p=2*stats::pt(-abs(med/pooled_se),df=df),
      att_multisplit_df=df,att_multisplit_reference="t_common_survey_df",
      att_multisplit_method="median_of_within_split_SE_squared_plus_squared_deviation",
      inference_status=if(nrow(d)>1L) "repeated_split_median_ATT_approximate_survey_inference" else
        "single_split_ATT_no_aggregation_gain",
      policy_aggregation_status="not_aggregated_use_per_seed_policy_outputs",
      seeds=paste(sort(as.integer(d$seed)),collapse=";"),
      multisplit_spec_md5=as.character(d$multisplit_spec_md5[1L]),
      multisplit_source_md5=as.character(d$multisplit_source_md5[1L]),
      multisplit_sample_md5=as.character(d$multisplit_sample_md5[1L]),
      stringsAsFactors=FALSE))
  })
  out <- do.call(rbind,ans)
  out <- out[order(out$multisplit_analysis_tag),,drop=FALSE]
  rownames(out) <- NULL
  out
}


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
    analysis_tag <- multiseed_analysis_tag(tag,seed)
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
  # version/source fingerprint invalidates an incompatible cache. Prefer compatible
  # caches, but never make a harmless cache miss fatal because the caller had
  # disabled construction stages.
  base_cfg$stages$run_read_wave1_phase <- TRUE
  base_cfg$stages$run_build_main_dataset_phase <- TRUE
  base_cfg$cache$use_cached_wave1 <- TRUE
  base_cfg$cache$use_cached_main_dataset <- TRUE
  if (is.null(seeds) || !length(seeds))
    stop("run_multiseed_att: no seeds supplied; set cfg$global$multiseed_seeds.", call. = FALSE)
  seeds <- validate_multiseed_ids(seeds)
  if (!identical(base_cfg$final_tmle$primary_estimand,"att"))
    stop("The repeated-split ATT runner requires primary_estimand='att'.",call.=FALSE)
  out_dir <- out_dir %||% base_cfg$global$output_dir
  ensure_output_dir(out_dir, cfg = base_cfg, test_write = TRUE)
  message(sprintf("\n===== STAGE: Multi-seed ATT aggregation and stability (%d seeds) =====", length(seeds)))
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
    # Archived prior artifacts remain as clearly named siblings inside the
    # multiseed root; permit only those explicit archive names (and the normal
    # run-log allowlist) while still rejecting every unknown stale entry.
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
      rows <- prepare_multiseed_att_rows(r,s,cfg_s,ms_sig)
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
      rows <- prepare_multiseed_att_rows(r,s,cfg_s,ms_sig)
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
  write_per_seed() # also regenerates a missing aggregate CSV on compatible resume
  agg <- aggregate_multiseed_att(all_rows,seeds,base_cfg$global$pipeline_seed)
  # Verify that exported aggregate-only values reproduce the result. CSV decimal
  # serialization is allowed a numerical tolerance, not asserted byte identity.
  disk_rows <- utils::read.csv(per_seed_csv,comment.char="#",check.names=FALSE,
                              stringsAsFactors=FALSE)
  # read.csv may infer an all-NA grouping column as logical; preserve its input
  # type for typed grouping hashes. Numeric inference is harmless for estimates.
  for (nm in names(all_rows)) {
    if (is.character(all_rows[[nm]])) disk_rows[[nm]] <- as.character(disk_rows[[nm]])
    if (is.numeric(all_rows[[nm]])) disk_rows[[nm]] <- as.numeric(disk_rows[[nm]])
    if (is.integer(all_rows[[nm]])) disk_rows[[nm]] <- as.integer(disk_rows[[nm]])
    if (is.logical(all_rows[[nm]])) disk_rows[[nm]] <- as.logical(disk_rows[[nm]])
  }
  from_disk <- aggregate_multiseed_att(disk_rows,seeds,base_cfg$global$pipeline_seed)
  if (!isTRUE(all.equal(agg,from_disk,tolerance=1e-10,check.attributes=FALSE)))
    stop("Multi-split aggregate does not reconcile with the exported per-seed table.",call.=FALSE)
  rownames(agg) <- NULL
  write_provenance_csv_at_path(agg, aggregate_cfg, agg_csv,
                               "ms_sum.csv", overwrite = TRUE)
  write_multiseed_selection_stability(selection_collected, out_dir, aggregate_cfg)
  verify_frozen_source_unchanged(aggregate_cfg)
  message(sprintf("  [multiseed] wrote per-seed (%s), aggregate (%s), and selection-stability artifacts.",
                  basename(per_seed_csv), basename(agg_csv)))
  message("  [multiseed] Optional runner reports split-aggregated ATT; designated seed is a comparator.")
  message("  [multiseed] Policy ratios/elasticities remain per-seed; no pooled policy CI is implied.")
  invisible(list(per_seed = all_rows, aggregate = agg))
}


