# Generated from the reviewed v8.28 production source.
# Original lines: 10370-13032.
# Module role: Peer-review diagnostics.
# See docs/REFACTOR_AUDIT.md for the exact transformation record.

# 9) PEER-REVIEW DIAGNOSTICS
# =============================================================================
# Plain-English role: produce the standard set of figures and tables a
# reviewer will want to see alongside the headline effect. Covariate balance,
# propensity distributions, learner weights, overlap products, fold timings,
# QQ plot of the cluster-level EIC, and a CONSORT-style sample-flow CSV.


write_diag_csv <- function(x, cfg, out_dir, filename) {
  path <- build_unique_diag_path(cfg, out_dir, filename)
  write_provenance_csv_at_path(x, cfg, path, filename, row.names = FALSE)
  invisible(path)
}


read_output_metadata <- function(path, max_lines = 40L) {
  lines <- readLines(path, n = max_lines, warn = FALSE)
  lines <- lines[startsWith(lines, "#")]
  if (!length(lines)) return(setNames(character(0), character(0)))
  body <- sub("^#[[:space:]]*", "", lines)
  key <- trimws(sub(":.*$", "", body))
  val <- trimws(sub("^[^:]+:[[:space:]]*", "", body))
  stats::setNames(val, key)
}

build_output_inventory <- function(cfg) {
  root <- cfg$global$output_dir
  files <- list.files(root, recursive = TRUE, full.names = TRUE, all.files = TRUE,
                      no.. = TRUE)
  files <- files[file.info(files)$isdir %in% FALSE]
  if (!length(files)) return(data.frame())
  rows <- lapply(files, function(path) {
    rel <- substring(normalizePath(path, winslash = "/", mustWork = TRUE),
                     nchar(normalizePath(root, winslash = "/", mustWork = TRUE)) + 2L)
    ext <- tolower(tools::file_ext(path))
    meta <- if (ext == "csv") read_output_metadata(path) else character(0)
    dims <- c(NA_integer_, NA_integer_)
    if (ext == "csv") {
      z <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE,
        check.names = FALSE, comment.char = "#"), error = function(e) NULL)
      if (!is.null(z)) dims <- c(nrow(z), ncol(z))
    }
    data.frame(
      relative_path = rel,
      extension = ext,
      bytes = unname(file.info(path)$size),
      md5 = unname(tools::md5sum(path)),
      n_rows = dims[1L], n_columns = dims[2L],
      run_id = unname(meta["Run ID"] %||% NA_character_),
      script_md5 = unname(meta["Script MD5"] %||% NA_character_),
      analysis_spec_md5 = unname(meta["Analysis spec"] %||% NA_character_),
      resolved_run_config_md5 = unname(meta["Run config"] %||% NA_character_),
      outcome_definition = unname(meta["Outcome def"] %||% NA_character_),
      stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

validate_output_bundle <- function(cfg, inventory = NULL) {
  if (is.null(inventory)) inventory <- build_output_inventory(cfg)
  csv <- inventory[inventory$extension == "csv", , drop = FALSE]
  if (!nrow(csv)) stop("Output-bundle validation found no CSV outputs.", call. = FALSE)
  expected <- c(
    run_id = cfg$global$run_id,
    script_md5 = get_frozen_source_fingerprint(cfg)$md5,
    analysis_spec_md5 = get_frozen_config_hash(cfg, "analysis"),
    resolved_run_config_md5 = get_frozen_config_hash(cfg, "resolved"),
    outcome_definition = cfg$provenance$outcome_definition)
  for (nm in names(expected)) {
    vals <- csv[[nm]]
    missing <- is.na(vals) | !nzchar(trimws(vals))
    if (any(missing))
      stop("Output-bundle validation found CSV file(s) without ", nm, ": ",
           paste(csv$relative_path[missing], collapse = ", "), call. = FALSE)
    bad <- vals != expected[[nm]]
    if (any(bad))
      stop("Output-bundle validation found mixed ", nm, " values in: ",
           paste(csv$relative_path[bad], collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

build_diagnostic_fit_bundle <- function(cfg, main_df, tmle_fit) {
  if (is.null(tmle_fit) || is.null(tmle_fit$att_components))
    stop("Diagnostic bundle requires a completed ATT fit.", call. = FALSE)
  ac <- tmle_fit$att_components
  n <- nrow(main_df)
  id_main <- main_df[[cfg$analysis$id_var]]
  id_fit <- tmle_fit$respondent_ids
  if (is.null(id_fit) || length(id_fit) != n ||
      !identical(as.character(id_main), as.character(id_fit)))
    stop("Diagnostic bundle row order does not match the fitted respondent IDs.",
         call. = FALSE)
  required_vectors <- list(
    outer_fold = tmle_fit$outer_fold, treatment = ac$A,
    outcome_observed = ac$delta_Y, survey_weight = ac$weights,
    cluster = ac$cluster, strata = ac$strata, outcome_raw = ac$Y_raw,
    outcome_bounded = ac$Y_bounded_orig, g = ac$gn, g_raw = ac$gn_raw,
    pi_A = ac$pi_AW, pi_A_raw = ac$pi_AW_raw, pi_1 = ac$pi_1W,
    pi_0 = ac$pi_0W, Q1_targeted = ac$Qbar1W_orig,
    Q0_targeted = ac$Qbar0W_orig, Q_A_targeted = ac$QbarAW_orig,
    att_influence_function = ac$D_att,
    att_mu1_influence_function = ac$D_mu1,
    att_mu0_influence_function = ac$D_mu0)
  bad_length <- names(required_vectors)[vapply(
    required_vectors, function(z) is.null(z) || length(z) != n, logical(1))]
  if (length(bad_length))
    stop("Diagnostic bundle has missing or misaligned fitted vectors: ",
         paste(bad_length, collapse = ", "), call. = FALSE)
  list(
    restricted_use_notice = paste0(
      "Contains respondent- and cluster-linked derived Add Health data. ",
      "Keep inside the restricted-use environment."),
    provenance = cfg$provenance,
    run_id = cfg$global$run_id,
    result = tmle_fit$result,
    respondent_id = main_df[[cfg$analysis$id_var]],
    outer_fold = tmle_fit$outer_fold,
    treatment = ac$A,
    outcome_observed = ac$delta_Y,
    survey_weight = ac$weights,
    cluster = ac$cluster,
    strata = ac$strata,
    outcome_raw = ac$Y_raw,
    outcome_bounded = ac$Y_bounded_orig,
    g = ac$gn, g_raw = ac$gn_raw,
    pi_A = ac$pi_AW, pi_A_raw = ac$pi_AW_raw,
    pi_1 = ac$pi_1W, pi_0 = ac$pi_0W,
    Q1_targeted = ac$Qbar1W_orig,
    Q0_targeted = ac$Qbar0W_orig,
    Q_A_targeted = ac$QbarAW_orig,
    att_influence_function = ac$D_att,
    att_mu1_influence_function = ac$D_mu1,
    att_mu0_influence_function = ac$D_mu0,
    run_manifest = tmle_fit$run_manifest,
    superlearner_log = tmle_fit$sl_log,
    selection_log = tmle_fit$selection_log,
    selected_by_fold = tmle_fit$selected_by_fold,
    survey_design_frame = attr(main_df, "survey_design_frame", exact = TRUE))
}

build_manuscript_summary <- function(cfg, main_df, tmle_fit) {
  rr <- tmle_fit$result[1L, , drop = FALSE]
  flow <- attr(main_df, "sample_flow") %||% list()
  for (nm in names(flow)) rr[[paste0("flow_", nm)]] <- flow[[nm]]
  rr$run_id <- cfg$global$run_id
  rr$pipeline_version <- cfg$global$version
  rr$script_md5 <- get_frozen_source_fingerprint(cfg)$md5
  rr$analysis_spec_md5 <- get_frozen_config_hash(cfg, "analysis")
  rr$resolved_run_config_md5 <- get_frozen_config_hash(cfg, "resolved")
  rr$outcome_definition <- cfg$provenance$outcome_definition
  rr
}


kish_eff_n <- function(w) {
  w <- as.numeric(w)
  w <- w[is.finite(w) & w > 0]
  if (length(w) == 0L) return(NA_real_)
  sum(w)^2 / sum(w^2)
}

weighted_mean_safe <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  stats::weighted.mean(x[ok], w[ok])
}

weighted_var_safe <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (sum(ok) < 2L) return(NA_real_)
  ww <- w[ok] / sum(w[ok])
  mu <- sum(ww * x[ok])
  sum(ww * (x[ok] - mu)^2)
}

weighted_auc_tie_corrected <- function(y, score, w) {
  y <- as.integer(y)
  score <- as.numeric(score)
  w <- as.numeric(w)
  ok <- !is.na(y) & y %in% c(0L, 1L) & is.finite(score) &
    is.finite(w) & w > 0
  y <- y[ok]; score <- score[ok]; w <- w[ok]
  if (!length(y) || length(unique(y)) < 2L) return(NA_real_)
  pos_total <- sum(w[y == 1L])
  neg_total <- sum(w[y == 0L])
  den <- pos_total * neg_total
  if (!is.finite(den) || den <= 0) return(NA_real_)

  ord <- order(score, na.last = NA)
  y <- y[ord]; score <- score[ord]; w <- w[ord]
  runs <- rle(score)
  ends <- cumsum(runs$lengths)
  starts <- c(1L, head(ends, -1L) + 1L)
  neg_below <- 0
  concordant <- 0
  for (jj in seq_along(starts)) {
    ii <- starts[jj]:ends[jj]
    pos_w <- sum(w[ii][y[ii] == 1L])
    neg_w <- sum(w[ii][y[ii] == 0L])
    concordant <- concordant + pos_w * (neg_below + 0.5 * neg_w)
    neg_below <- neg_below + neg_w
  }
  pmin(pmax(concordant / den, 0), 1)
}

capture_optional_diagnostic <- function(label, fun) {
  started <- proc.time()[3]
  tryCatch({
    value <- fun()
    list(
      value = value,
      status = data.frame(
        diagnostic = as.character(label), status = "success",
        seconds = proc.time()[3] - started, error = NA_character_,
        stringsAsFactors = FALSE))
  }, error = function(e) {
    message(sprintf("WARNING: Optional diagnostic '%s' failed: %s", label,
                    conditionMessage(e)))
    list(
      value = NULL,
      status = data.frame(
        diagnostic = as.character(label), status = "failed",
        seconds = proc.time()[3] - started,
        error = conditionMessage(e), stringsAsFactors = FALSE))
  })
}

balance_factor_levels <- function(x, group, w_pre, w_post, variable, comparison) {
  group <- as.integer(group)
  if (!all(group %in% c(0L, 1L)))
    stop("Balance group must be coded 0/1.", call. = FALSE)
  if (is.numeric(x) || is.integer(x)) return(data.frame())

  x_chr <- as.character(x)
  x_chr[is.na(x_chr)] <- "Missing"
  ff <- factor(x_chr)
  levs <- levels(ff)
  if (length(levs) < 2L) return(data.frame())

  one_set <- function(w, stage) {
    rows <- lapply(levs, function(lev) {
      z <- as.integer(ff == lev)
      ok1 <- group == 1L & is.finite(w) & w > 0
      ok0 <- group == 0L & is.finite(w) & w > 0
      if (sum(ok1) < 5L || sum(ok0) < 5L) {
        p1 <- p0 <- smd <- NA_real_
      } else {
        p1 <- stats::weighted.mean(z[ok1], w[ok1])
        p0 <- stats::weighted.mean(z[ok0], w[ok0])
        pp <- (p1 + p0) / 2
        sp <- sqrt(pp * (1 - pp))
        smd <- if (is.finite(sp) && sp > 0) (p1 - p0) / sp else NA_real_
      }
      data.frame(
        comparison = comparison, variable = variable, level = lev,
        stage = stage, prevalence_group1 = p1, prevalence_group0 = p0,
        smd = smd, abs_smd = abs(smd), stringsAsFactors = FALSE)
    })
    do.call(rbind, rows)
  }
  rbind(one_set(w_pre, "pre"), one_set(w_post, "post"))
}

balance_one_variable <- function(x, group, w_pre, w_post, variable) {
  group <- as.integer(group)
  if (!all(group %in% c(0L, 1L)))
    stop("Balance group must be coded 0/1.", call. = FALSE)
  if (is.numeric(x) || is.integer(x)) {
    xx <- suppressWarnings(as.numeric(x))
    observed <- is.finite(xx)
    smd_num <- function(w) {
      ok1 <- group == 1L & observed & is.finite(w) & w > 0
      ok0 <- group == 0L & observed & is.finite(w) & w > 0
      if (sum(ok1) < 5L || sum(ok0) < 5L) return(NA_real_)
      m1 <- stats::weighted.mean(xx[ok1], w[ok1])
      m0 <- stats::weighted.mean(xx[ok0], w[ok0])
      v1 <- weighted_var_safe(xx[ok1], w[ok1])
      v0 <- weighted_var_safe(xx[ok0], w[ok0])
      sp <- sqrt(mean(c(v1, v0), na.rm = TRUE))
      if (!is.finite(sp) || sp <= 0) return(NA_real_)
      (m1 - m0) / sp
    }
    missing_smd <- function(w) {
      miss <- as.integer(!observed)
      ok1 <- group == 1L & is.finite(w) & w > 0
      ok0 <- group == 0L & is.finite(w) & w > 0
      if (sum(ok1) < 5L || sum(ok0) < 5L) return(NA_real_)
      p1 <- stats::weighted.mean(miss[ok1], w[ok1])
      p0 <- stats::weighted.mean(miss[ok0], w[ok0])
      pp <- (p1 + p0) / 2
      sp <- sqrt(pp * (1 - pp))
      if (!is.finite(sp) || sp <= 0) return(if (isTRUE(all.equal(p1, p0))) 0 else NA_real_)
      (p1 - p0) / sp
    }
    obs1 <- group == 1L & observed
    obs0 <- group == 0L & observed
    ess <- function(mask, w) kish_eff_n(w[mask & is.finite(w) & w > 0])
    ess_pre_1 <- ess(obs1, w_pre); ess_pre_0 <- ess(obs0, w_pre)
    ess_post_1 <- ess(obs1, w_post); ess_post_0 <- ess(obs0, w_post)
    post_ess <- c(ess_post_1, ess_post_0)
    sparse <- sum(obs1) < 5L || sum(obs0) < 5L ||
      any(!is.finite(post_ess) | post_ess < 5)
    return(data.frame(
      variable = variable, type = "numeric",
      smd_pre = smd_num(w_pre), smd_post = smd_num(w_post),
      max_level = NA_character_,
      n_observed_group1 = sum(obs1), n_observed_group0 = sum(obs0),
      ess_observed_group1_pre = ess_pre_1,
      ess_observed_group0_pre = ess_pre_0,
      ess_observed_group1_post = ess_post_1,
      ess_observed_group0_post = ess_post_0,
      missing_smd_pre = missing_smd(w_pre),
      missing_smd_post = missing_smd(w_post),
      sparse_observed_support = sparse,
      stringsAsFactors = FALSE))
  }

  level_rows <- balance_factor_levels(x, group, w_pre, w_post, variable, "")
  if (!nrow(level_rows)) return(NULL)
  pre <- level_rows[level_rows$stage == "pre", , drop = FALSE]
  post <- level_rows[level_rows$stage == "post", , drop = FALSE]
  idx_pre <- if (any(is.finite(pre$smd))) which.max(pre$abs_smd) else NA_integer_
  idx_post <- if (any(is.finite(post$smd))) which.max(post$abs_smd) else NA_integer_
  data.frame(
    variable = variable, type = "factor",
    smd_pre = if (is.na(idx_pre)) NA_real_ else pre$smd[idx_pre],
    smd_post = if (is.na(idx_post)) NA_real_ else post$smd[idx_post],
    max_level = if (is.na(idx_post)) NA_character_ else post$level[idx_post],
    n_observed_group1 = NA_integer_, n_observed_group0 = NA_integer_,
    ess_observed_group1_pre = NA_real_, ess_observed_group0_pre = NA_real_,
    ess_observed_group1_post = NA_real_, ess_observed_group0_post = NA_real_,
    missing_smd_pre = NA_real_, missing_smd_post = NA_real_,
    sparse_observed_support = NA,
    stringsAsFactors = FALSE)
}

prepare_balance_variable <- function(x, rule, cfg_pre) {
  masks <- missing_masks_from_rule(x, rule)
  if (isTRUE(rule$as_factor)) {
    xc <- if (isTRUE(rule$numeric_coded)) as.character(masks$numeric) else as.character(x)
    xc[masks$general] <- cfg_pre$factor_missing_label %||% "Missing"
    xc[masks$skip] <- cfg_pre$factor_skip_label %||% "Skip"
    return(factor(xc))
  }
  z <- masks$numeric
  z[masks$general | masks$skip] <- NA_real_
  z
}

make_balance_table <- function(df, cfg, group, post_weights, label,
                               priority_vars = character(0)) {
  cand_all <- get_candidate_vars(df, cfg)
  priority_vars <- unique(intersect(as.character(priority_vars), cand_all))
  w_pre <- suppressWarnings(as.numeric(df[[cfg$analysis$weight_var]]))
  w_pre[!is.finite(w_pre) | w_pre <= 0] <- NA_real_
  w_post <- as.numeric(post_weights)
  w_post[!is.finite(w_post) | w_post <= 0] <- NA_real_
  if (length(group) != nrow(df) || length(w_post) != nrow(df))
    stop("Balance inputs have inconsistent row counts.", call. = FALSE)

  scan_started <- proc.time()[3]
  progress_every <- as.integer(cfg$diagnostics$balance_progress_every %||% 500L)
  if (!is.finite(progress_every) || progress_every < 1L) progress_every <- 500L
  msg(sprintf("  [diag] %s: scanning balance for %d eligible candidates...",
              label, length(cand_all)), cfg = cfg)

  # Cache each candidate's summary for this exact (group, pre-weight,
  # post-weight) comparison. The detailed table reuses these summaries rather
  # than recalculating the same balance statistic after ranking candidates.
  all_scan_rows <- vector("list", length(cand_all))
  all_scan_details <- vector("list", length(cand_all))
  names(all_scan_details) <- cand_all
  for (jj in seq_along(cand_all)) {
    v <- cand_all[jj]
    rule <- get_missing_rule(df[[v]], cfg$preprocessing, variable_name = v)
    x_bal <- prepare_balance_variable(df[[v]], rule, cfg$preprocessing)
    ans <- tryCatch(
      balance_one_variable(x_bal, group, w_pre, w_post, v),
      error = function(e) NULL)
    all_scan_details[v] <- list(ans)
    if (is.null(ans)) {
      all_scan_rows[[jj]] <- data.frame(
        variable = v, smd_pre = NA_real_, smd_post = NA_real_,
        abs_smd_pre = NA_real_, abs_smd_post = NA_real_,
        max_abs_smd = NA_real_, stringsAsFactors = FALSE)
    } else {
      vals <- abs(c(ans$smd_pre[1L], ans$smd_post[1L]))
      mx <- if (any(is.finite(vals))) max(vals[is.finite(vals)]) else NA_real_
      all_scan_rows[[jj]] <- data.frame(
        variable = v, smd_pre = ans$smd_pre[1L],
        smd_post = ans$smd_post[1L],
        abs_smd_pre = abs(ans$smd_pre[1L]),
        abs_smd_post = abs(ans$smd_post[1L]),
        max_abs_smd = mx, stringsAsFactors = FALSE)
    }
    if (jj %% progress_every == 0L || jj == length(cand_all)) {
      msg(sprintf("  [diag] %s: completed %d/%d candidate balance summaries (%.1fs).",
                  label, jj, length(cand_all), proc.time()[3] - scan_started),
          cfg = cfg)
    }
  }
  all_scan <- do.call(rbind, all_scan_rows)
  ranking_score <- all_scan$max_abs_smd
  ranking_score[!is.finite(ranking_score)] <- -Inf
  scan_seconds <- proc.time()[3] - scan_started

  remaining <- setdiff(cand_all, priority_vars)
  max_bal <- as.integer(cfg$diagnostics$max_balance_variables %||% length(cand_all))
  n_fill <- max(0L, max_bal - length(priority_vars))
  rem_score <- ranking_score[match(remaining, all_scan$variable)]
  ranked_remaining <- remaining[order(rem_score, decreasing = TRUE, na.last = TRUE)]
  cand <- unique(c(priority_vars, head(ranked_remaining, n_fill)))
  if (length(priority_vars) > max_bal) {
    message(sprintf(
      "  [diag] Balance table for %s retains all %d priority variables, exceeding diagnostics$max_balance_variables=%d.",
      label, length(priority_vars), max_bal))
  } else if (length(cand_all) > length(cand)) {
    message(sprintf(
      "  [diag] Balance table for %s includes all %d priority variables plus %d additional variables (%d of %d total candidates).",
      label, length(priority_vars), length(cand) - length(priority_vars),
      length(cand), length(cand_all)))
  }

  rows <- list(); level_rows <- list()
  for (v in cand) {
    ans <- all_scan_details[[v]]
    if (is.null(ans)) {
      rule <- get_missing_rule(df[[v]], cfg$preprocessing, variable_name = v)
      x_bal <- prepare_balance_variable(df[[v]], rule, cfg$preprocessing)
      ans <- tryCatch(
        balance_one_variable(x_bal, group, w_pre, w_post, v),
        error = function(e) stop(sprintf(
          "Balance calculation failed for '%s' in %s: %s",
          v, label, conditionMessage(e)), call. = FALSE))
    }
    if (!is.null(ans)) rows[[v]] <- ans

    # Factor-level output is intentionally computed only for the detailed
    # subset. Numeric/factor summary rows above are reused from the all-scan.
    rule <- get_missing_rule(df[[v]], cfg$preprocessing, variable_name = v)
    x_bal <- prepare_balance_variable(df[[v]], rule, cfg$preprocessing)
    if (!(is.numeric(x_bal) || is.integer(x_bal))) {
      lev <- tryCatch(
        balance_factor_levels(x_bal, group, w_pre, w_post, v, label),
        error = function(e) stop(sprintf(
          "Factor-level balance failed for '%s' in %s: %s",
          v, label, conditionMessage(e)), call. = FALSE))
      if (nrow(lev)) level_rows[[v]] <- lev
    }
  }
  out <- if (length(rows)) do.call(rbind, rows) else NULL
  levels_out <- if (length(level_rows)) do.call(rbind, level_rows) else data.frame()
  if (is.null(out) || nrow(out) == 0L) {
    out <- data.frame(
      comparison = character(0), variable = character(0), type = character(0),
      smd_pre = numeric(0), smd_post = numeric(0),
      abs_smd_pre = numeric(0), abs_smd_post = numeric(0),
      max_level = character(0),
      n_observed_group1 = integer(0), n_observed_group0 = integer(0),
      ess_observed_group1_pre = numeric(0),
      ess_observed_group0_pre = numeric(0),
      ess_observed_group1_post = numeric(0),
      ess_observed_group0_post = numeric(0),
      missing_smd_pre = numeric(0), missing_smd_post = numeric(0),
      sparse_observed_support = logical(0),
      love_plot_eligible = logical(0),
      love_plot_exclusion_reason = character(0))
    attr(out, "factor_level_balance") <- levels_out
    attr(out, "all_candidate_balance_scan") <- all_scan
    attr(out, "balance_scan_seconds") <- scan_seconds
    attr(out, "balance_scan_n_candidates") <- length(cand_all)
    return(out)
  }
  out$comparison <- label
  out$abs_smd_pre <- abs(out$smd_pre)
  out$abs_smd_post <- abs(out$smd_post)
  out <- out[, c(
    "comparison", "variable", "type", "smd_pre", "smd_post",
    "abs_smd_pre", "abs_smd_post", "max_level",
    "n_observed_group1", "n_observed_group0",
    "ess_observed_group1_pre", "ess_observed_group0_pre",
    "ess_observed_group1_post", "ess_observed_group0_post",
    "missing_smd_pre", "missing_smd_post",
    "sparse_observed_support")]
  max_abs <- pmax(out$abs_smd_pre, out$abs_smd_post, na.rm = TRUE)
  max_abs[!is.finite(max_abs)] <- -Inf
  out <- out[order(-max_abs), , drop = FALSE]
  attr(out, "factor_level_balance") <- levels_out
  attr(out, "all_candidate_balance_scan") <- all_scan
  attr(out, "balance_scan_seconds") <- scan_seconds
  attr(out, "balance_scan_n_candidates") <- length(cand_all)
  out
}

make_balance_table_for_vars <- function(df, cfg, vars, group, weights, label) {
  vars <- unique(intersect(as.character(vars), names(df)))
  if (!length(vars)) return(data.frame())
  group <- as.integer(group)
  weights <- as.numeric(weights)
  if (length(group) != nrow(df) || length(weights) != nrow(df) ||
      anyNA(group) || any(!group %in% c(0L, 1L)))
    stop("Selected-variable balance inputs are invalid.", call. = FALSE)
  rows <- lapply(vars, function(v) {
    rule <- get_missing_rule(df[[v]], cfg$preprocessing, variable_name = v)
    x_bal <- prepare_balance_variable(df[[v]], rule, cfg$preprocessing)
    ans <- balance_one_variable(x_bal, group, weights, weights, v)
    if (is.null(ans)) return(NULL)
    ans$comparison <- label
    ans$abs_smd <- abs(ans$smd_pre)
    ans
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

build_wave2_completion_diagnostic <- function(df_with_wave2, complete_cesd, cfg) {
  w <- suppressWarnings(as.numeric(df_with_wave2[[cfg$analysis$weight_var]]))
  ok_w <- is.finite(w) & w > 0
  complete_cesd <- as.integer(complete_cesd)
  if (length(complete_cesd) != nrow(df_with_wave2) ||
      anyNA(complete_cesd) || any(!complete_cesd %in% c(0L, 1L)))
    stop("Wave-II completion indicator is invalid.", call. = FALSE)
  balance <- make_balance_table_for_vars(
    df_with_wave2, cfg, get_mandatory_W(cfg), complete_cesd, w,
    "Wave II Feelings Scale completers vs noncompleters")
  summary <- data.frame(
    n_total = nrow(df_with_wave2),
    n_complete = sum(complete_cesd == 1L),
    n_incomplete = sum(complete_cesd == 0L),
    weighted_completion_rate = if (any(ok_w))
      stats::weighted.mean(complete_cesd[ok_w], w[ok_w]) else NA_real_,
    weighted_ess_total = kish_eff_n(w[ok_w]),
    weighted_ess_complete = kish_eff_n(w[ok_w & complete_cesd == 1L]),
    weighted_ess_incomplete = kish_eff_n(w[ok_w & complete_cesd == 0L]),
    stringsAsFactors = FALSE)
  list(balance = balance, summary = summary)
}

parse_named_metric_string <- function(s, fold, nuisance, value_name) {
  if (is.na(s) || !nzchar(s)) {
    out <- data.frame(fold = integer(0), nuisance = character(0),
                      learner = character(0), value = numeric(0))
    names(out)[names(out) == "value"] <- value_name
    return(out)
  }
  parts <- strsplit(as.character(s), ";", fixed = TRUE)[[1]]
  pieces <- strsplit(parts, "=", fixed = TRUE)
  out <- data.frame(fold = fold, nuisance = nuisance,
                    learner = vapply(pieces, `[`, character(1), 1L),
                    value = suppressWarnings(as.numeric(vapply(pieces, `[`, character(1), 2L))),
                    stringsAsFactors = FALSE)
  names(out)[names(out) == "value"] <- value_name
  out
}

make_sl_weight_long <- function(sl_log) {
  if (is.null(sl_log) || nrow(sl_log) == 0L) return(data.frame())
  rows <- list()
  for (i in seq_len(nrow(sl_log))) {
    rows[[length(rows) + 1L]] <- parse_named_metric_string(sl_log$Q_coef[i], sl_log$fold[i], "Q", "weight")
    rows[[length(rows) + 1L]] <- parse_named_metric_string(sl_log$g_coef[i], sl_log$fold[i], "g", "weight")
    rows[[length(rows) + 1L]] <- parse_named_metric_string(sl_log$pi_coef[i], sl_log$fold[i], "pi", "weight")
  }
  out <- do.call(rbind, rows)
  out <- out[is.finite(out$weight), , drop = FALSE]
  out
}

make_sl_risk_long <- function(sl_log) {
  if (is.null(sl_log) || nrow(sl_log) == 0L) return(data.frame())
  rows <- list()
  for (i in seq_len(nrow(sl_log))) {
    rows[[length(rows) + 1L]] <- parse_named_metric_string(
      sl_log$Q_cv_risk[i], sl_log$fold[i], "Q", "cv_risk")
    rows[[length(rows) + 1L]] <- parse_named_metric_string(
      sl_log$g_cv_risk[i], sl_log$fold[i], "g", "cv_risk")
    rows[[length(rows) + 1L]] <- parse_named_metric_string(
      sl_log$pi_cv_risk[i], sl_log$fold[i], "pi", "cv_risk")
  }
  out <- do.call(rbind, rows)
  out <- out[is.finite(out$cv_risk), , drop = FALSE]
  out
}

make_outer_validation_summary <- function(sl_log, tmle_fit) {
  if (is.null(sl_log) || !nrow(sl_log)) return(data.frame())
  metrics <- c("Q_outer_mse", "Q_outer_brier", "g_outer_brier", "g_outer_logloss",
               "pi_outer_brier", "pi_outer_logloss")
  metrics <- intersect(metrics, names(sl_log))
  rows <- lapply(metrics, function(nm) {
    z <- suppressWarnings(as.numeric(sl_log[[nm]])); z <- z[is.finite(z)]
    data.frame(section = "outer_fold_ensemble_loss", nuisance = sub("_outer.*$", "", nm),
      metric = nm, value = if (length(z)) mean(z) else NA_real_,
      sd_across_folds = if (length(z) > 1L) stats::sd(z) else NA_real_,
      min_across_folds = if (length(z)) min(z) else NA_real_,
      max_across_folds = if (length(z)) max(z) else NA_real_, stringsAsFactors = FALSE)
  })
  ac <- tmle_fit$att_components %||% list()
  binary_calibration <- function(y, p, w, label) {
    ok <- is.finite(y) & is.finite(p) & is.finite(w) & w > 0 & y %in% c(0, 1)
    y <- y[ok]; p <- p[ok]; w <- normalize_positive_weights(w[ok], sum(ok), paste0(label, " calibration weights"))
    p <- pmin(pmax(p, 1e-8), 1 - 1e-8); lp <- stats::qlogis(p)
    if (length(y) < 20L || length(unique(y)) < 2L || stats::sd(lp) <= 0)
      return(c(intercept = NA_real_, slope = NA_real_))
    fit <- suppressWarnings(tryCatch(stats::glm(y ~ lp, family = stats::binomial(), weights = w), error = function(e) NULL))
    if (is.null(fit) || length(stats::coef(fit)) < 2L) c(intercept = NA_real_, slope = NA_real_) else
      c(intercept = unname(stats::coef(fit)[1L]), slope = unname(stats::coef(fit)[2L]))
  }
  if (length(ac$A) && length(ac$gn_raw)) {
    cal <- binary_calibration(ac$A, ac$gn_raw, ac$weights, "g")
    rows[[length(rows)+1L]] <- data.frame(section="overall_calibration", nuisance="g", metric="calibration_intercept", value=cal["intercept"], sd_across_folds=NA_real_, min_across_folds=NA_real_, max_across_folds=NA_real_)
    rows[[length(rows)+1L]] <- data.frame(section="overall_calibration", nuisance="g", metric="calibration_slope", value=cal["slope"], sd_across_folds=NA_real_, min_across_folds=NA_real_, max_across_folds=NA_real_)
  }
  if (length(ac$delta_Y) && length(ac$pi_AW_raw)) {
    cal <- binary_calibration(ac$delta_Y, ac$pi_AW_raw, ac$weights, "pi")
    rows[[length(rows)+1L]] <- data.frame(section="overall_calibration", nuisance="pi", metric="calibration_intercept", value=cal["intercept"], sd_across_folds=NA_real_, min_across_folds=NA_real_, max_across_folds=NA_real_)
    rows[[length(rows)+1L]] <- data.frame(section="overall_calibration", nuisance="pi", metric="calibration_slope", value=cal["slope"], sd_across_folds=NA_real_, min_across_folds=NA_real_, max_across_folds=NA_real_)
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# Confounding-sensitivity contour, extending the single E-value into a curve.
# Given the observed (approximate) risk ratio, returns the minimum
# confounder-outcome association RR_UD needed to explain the effect away across a
# grid of confounder-exposure associations RR_EU, via the VanderWeele-Ding bias
# factor B = (RR_EU*RR_UD)/(RR_EU+RR_UD-1). Explaining away requires B >= RR_obs,
# so RR_UD = RR_obs*(RR_EU-1)/(RR_EU-RR_obs) for RR_EU > RR_obs; the symmetric
# point RR_EU = RR_UD reproduces the E-value. RR_EU <= RR_obs cannot suffice.
make_evalue_contour <- function(approx_rr, rr_grid = c(1.25, 1.5, 1.75, 2, 2.5, 3, 4, 5)) {
  rr <- suppressWarnings(as.numeric(approx_rr)[1])
  if (is.finite(rr) && rr < 1) rr <- 1 / rr             # express away from null
  if (!is.finite(rr) || rr <= 1)
    return(data.frame(rr_eu = rr_grid, rr_ud_min = NA_real_, observed_rr = rr,
                      note = "Estimate at/below null on the RR scale; no contour.",
                      stringsAsFactors = FALSE))
  rr_ud <- vapply(rr_grid, function(reu)
    if (is.finite(reu) && reu > rr) rr * (reu - 1) / (reu - rr) else NA_real_,
    numeric(1))
  data.frame(rr_eu = rr_grid, rr_ud_min = rr_ud, observed_rr = rr,
             note = "Min confounder-outcome RR to explain away the effect at each confounder-exposure RR (NA where RR_EU <= observed RR).",
             stringsAsFactors = FALSE)
}

make_evalue_approx <- function(estimate, ci_lower, ci_upper, y, weights = NULL, baseline_mean = NULL) {
  if (is.null(weights)) weights <- rep(1, length(y))
  ok <- is.finite(y) & is.finite(weights) & weights > 0
  y <- as.numeric(y[ok]); weights <- as.numeric(weights[ok])
  # Binary outcome: the ATT estimate is a risk difference. Convert to an
  # approximate risk ratio using the sample baseline risk, then apply the
  # standard VanderWeele E-value formula RR* + sqrt(RR*(RR*-1)) on RR* >= 1.
  is_binary <- length(unique(y)) <= 2L && all(y %in% c(0, 1))
  if (is_binary) {
    p0 <- if (is.finite(baseline_mean %||% NA_real_))
      as.numeric(baseline_mean) else stats::weighted.mean(y, weights)
    if (!is.finite(p0) || p0 <= 0 || p0 >= 1) {
      return(data.frame(effect_scale = "binary_risk_difference",
                        std_effect = NA_real_, approx_rr = NA_real_,
                        ci_min_abs_bound_sd = NA_real_,
                        evalue_point = NA_real_, evalue_ci = NA_real_,
                        note = "Binary outcome; baseline risk degenerate.",
                        stringsAsFactors = FALSE))
    }
    rd_to_rr <- function(rd) {
      p1 <- min(max(p0 + rd, 1e-6), 1 - 1e-6)
      p1 / p0
    }
    rr <- rd_to_rr(estimate)
    if (is.finite(rr) && rr < 1) rr <- 1 / rr           # express away from null
    ev <- if (is.finite(rr) && rr >= 1) rr + sqrt(rr * (rr - 1)) else NA_real_
    if (ci_lower <= 0 && ci_upper >= 0) {
      ev_ci <- 1                                         # CI crosses null
    } else {
      rd_near <- if (abs(ci_lower) < abs(ci_upper)) ci_lower else ci_upper
      rr_ci <- rd_to_rr(rd_near)
      if (is.finite(rr_ci) && rr_ci < 1) rr_ci <- 1 / rr_ci
      ev_ci <- if (is.finite(rr_ci) && rr_ci >= 1) rr_ci + sqrt(rr_ci * (rr_ci - 1)) else NA_real_
    }
    return(data.frame(effect_scale = "binary_risk_difference_to_rr",
                      std_effect = estimate, approx_rr = rr,
                      ci_min_abs_bound_sd = NA_real_,
                      evalue_point = ev, evalue_ci = ev_ci,
                      note = "Binary outcome; E-value from approximate risk ratio (baseline risk = sample mean).",
                      stringsAsFactors = FALSE))
  }
  sd_y <- sqrt(weighted_var_safe(y, weights))
  if (length(y) < 10L || !is.finite(sd_y) || sd_y <= 0) {
    return(data.frame(effect_scale = "continuous_approx", std_effect = NA_real_,
                      approx_rr = NA_real_, ci_min_abs_bound_sd = NA_real_,
                      evalue_point = NA_real_, evalue_ci = NA_real_,
                      note = "Outcome SD degenerate.", stringsAsFactors = FALSE))
  }
  d <- abs(estimate / sd_y)
  rr <- exp(0.91 * d)
  ev <- if (is.finite(rr) && rr >= 1) rr + sqrt(rr * (rr - 1)) else NA_real_
  # Closest confidence-limit bound to the null, expressed in SD units.
  # If the CI crosses the null, the CI-limit E-value is 1 by definition.
  ci_min_abs_bound_sd <- if (ci_lower <= 0 && ci_upper >= 0) {
    0
  } else {
    min(abs(ci_lower), abs(ci_upper)) / sd_y
  }
  rr_ci <- exp(0.91 * ci_min_abs_bound_sd)
  ev_ci <- if (ci_min_abs_bound_sd == 0) 1 else rr_ci + sqrt(rr_ci * (rr_ci - 1))
  data.frame(effect_scale = "continuous_approx_vanderweele_rr_transform",
             std_effect = estimate / sd_y, approx_rr = rr,
             ci_min_abs_bound_sd = ci_min_abs_bound_sd,
             evalue_point = ev, evalue_ci = ev_ci,
             note = "Approximation for continuous outcomes; interpret as supplemental sensitivity only.",
             stringsAsFactors = FALSE)
}

derive_love_plot_exclusions <- function(tmle_fit, cfg) {
  if (!isTRUE(cfg$diagnostics$exclude_too_missing_from_love_plots %||% TRUE) ||
      is.null(tmle_fit) || is.null(tmle_fit$selection_log)) return(character(0))
  sl <- tmle_fit$selection_log
  needed <- c("variable", "fold", "prefilter_reason")
  if (!all(needed %in% names(sl))) return(character(0))
  folds <- sort(unique(sl$fold[is.finite(sl$fold)]))
  if (!length(folds)) return(character(0))
  vars <- unique(sl$variable[sl$prefilter_reason == "too_missing"])
  vars[vapply(vars, function(v) {
    d <- sl[sl$variable == v, , drop = FALSE]
    reason_folds <- unique(d$fold[d$prefilter_reason == "too_missing"])
    selected_any <- if ("selected" %in% names(d)) any(d$selected %in% TRUE, na.rm = TRUE) else FALSE
    kept_any <- if ("kept_in_final_W" %in% names(d))
      any(d$kept_in_final_W %in% TRUE, na.rm = TRUE) else FALSE
    setequal(reason_folds, folds) && !selected_any && !kept_any
  }, logical(1))]
}

annotate_balance_for_love_plot <- function(tab, excluded_vars) {
  if (is.null(tab)) return(tab)
  factor_level_balance <- attr(tab, "factor_level_balance")
  tab$love_plot_eligible <- !(tab$variable %in% excluded_vars)
  tab$love_plot_exclusion_reason <- ifelse(
    tab$love_plot_eligible, NA_character_, "too_missing_in_every_screening_fold")
  attr(tab, "factor_level_balance") <- factor_level_balance
  tab
}

plot_love <- function(tab, path, title, n_top = 30L) {
  if (is.null(tab) || nrow(tab) == 0L) return(invisible(NULL))
  if ("love_plot_eligible" %in% names(tab))
    tab <- tab[is.na(tab$love_plot_eligible) | tab$love_plot_eligible, , drop = FALSE]
  finite_balance <- is.finite(tab$abs_smd_pre) | is.finite(tab$abs_smd_post)
  tab <- tab[finite_balance, , drop = FALSE]
  if (nrow(tab) == 0L) return(invisible(NULL))
  score <- pmax(tab$abs_smd_pre, tab$abs_smd_post, na.rm = TRUE)
  score[!is.finite(score)] <- -Inf
  ord <- order(score, decreasing = TRUE)
  top <- head(ord, n_top)
  tt <- tab[top, , drop = FALSE]
  x_max <- max(c(tt$abs_smd_pre, tt$abs_smd_post, 0.1), na.rm = TRUE)
  png(path, width = 1100, height = 900, res = 150)
  par(mar = c(4, 9, 3, 1))
  y <- seq_len(nrow(tt))
  plot(tt$abs_smd_pre, y, pch = 1, xlim = c(0, x_max),
       yaxt = "n", xlab = "Absolute standardized mean difference", ylab = "", main = title)
  points(tt$abs_smd_post, y, pch = 19)
  axis(2, at = y, labels = tt$variable, las = 2, cex.axis = 0.7)
  abline(v = 0.1, lty = 2)
  legend("topright", legend = c("Pre", "Post"), pch = c(1, 19), bty = "n")
  dev.off()
}


# Targeted audit of the conservative exact-code missingness classifier. This does
# not alter any values. It audits only protected variables, variables selected
# in at least one final fold, and the leading screened variables that actually
# matter for the fitted pipeline.
build_targeted_missing_code_audit <- function(main_df, cfg, tmle_fit = NULL) {
  protected <- cfg$final_tmle$protected_W %||% character(0)
  selected <- if (!is.null(tmle_fit) && !is.null(tmle_fit$selected_by_fold))
    unique(unlist(tmle_fit$selected_by_fold, use.names = FALSE)) else character(0)
  leading <- character(0)
  if (!is.null(tmle_fit) && !is.null(tmle_fit$selection_log) &&
      "variable" %in% names(tmle_fit$selection_log)) {
    sl <- tmle_fit$selection_log
    score_cols <- intersect(c("outcome_score", "delta_score", "exposure_score"), names(sl))
    if (length(score_cols)) {
      score_mat <- suppressWarnings(as.matrix(sl[, score_cols, drop = FALSE]))
      storage.mode(score_mat) <- "double"
      best <- apply(score_mat, 1L, function(z) if (all(!is.finite(z))) -Inf else max(z[is.finite(z)]))
      leading <- unique(sl$variable[order(best, decreasing = TRUE)][seq_len(min(100L, length(best)))])
    }
  }
  vars <- unique(c(protected, selected, leading))
  vars <- vars[vars %in% names(main_df)]
  if (!length(vars)) return(data.frame())
  rows <- lapply(vars, function(v) {
    x <- main_df[[v]]
    role <- paste(c(if (v %in% protected) "protected" else NULL,
                    if (v %in% selected) "selected" else NULL,
                    if (v %in% leading) "leading_screen" else NULL), collapse = ";")
    rule <- get_missing_rule(x, cfg$preprocessing, variable_name = v)
    masks <- missing_masks_from_rule(x, rule)
    z <- masks$numeric
    finite <- is.finite(z)
    flagged_general <- sort(unique(z[finite & masks$general]))
    flagged_skip <- sort(unique(z[finite & masks$skip]))
    data.frame(
      variable = v, role = role, original_class = class(x)[1],
      classifier_support_type = rule$support_type,
      questionnaire_source = isTRUE(rule$questionnaire_source),
      questionnaire_like = isTRUE(rule$questionnaire_like),
      categorical_questionnaire = isTRUE(rule$categorical_questionnaire),
      forced_factor = isTRUE(rule$forced_factor),
      known_codebook_overlap = v %in% (cfg$preprocessing$known_codebook_overlap_vars %||% character(0)),
      percentage_like = isTRUE(rule$percentage_like),
      dense_small_count = isTRUE(rule$dense_small_count),
      scheme_decision = rule$scheme_decision,
      recognized_families = paste(rule$recognized_families, collapse = ";"),
      classifier_reason = rule$reason,
      n = length(x), n_finite = sum(finite),
      n_native_or_nonfinite_missing = sum(is.na(x) | (isTRUE(rule$numeric_coded) & !is.finite(z))),
      n_exact_general_missing = sum(finite & masks$general),
      prop_exact_general_missing = mean(finite & masks$general),
      n_exact_skip = sum(finite & masks$skip),
      prop_exact_skip = mean(finite & masks$skip),
      exact_general_codes = paste(rule$general_codes, collapse = ";"),
      exact_skip_codes = paste(rule$skip_codes, collapse = ";"),
      observed_general_values = paste(utils::head(flagged_general, 100L), collapse = ";"),
      observed_skip_values = paste(utils::head(flagged_skip, 100L), collapse = ";"),
      range_min = if (any(finite)) min(z[finite]) else NA_real_,
      range_max = if (any(finite)) max(z[finite]) else NA_real_,
      stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}


# Raw-dollar tail/cap audit. The primary outcome is the canonical annual-dollar
# outcome capped at the configured upper quantile, while valid extreme raw
# earnings remain visible here and in full-refit q0.990/q1.000 sensitivities.
build_earnings_tail_cap_audit <- function(main_df, cfg, tmle_fit, A, w) {
  if (is.null(tmle_fit) || !identical(cfg$outcome$family, "Compensation")) return(data.frame())
  y <- suppressWarnings(as.numeric(main_df[[cfg$analysis$outcome_var]]))
  obs <- is.finite(y)
  cap <- tmle_fit$y_upper
  src <- if ("EarningsSource" %in% names(main_df)) as.character(main_df$EarningsSource) else rep("unknown", length(y))
  cl <- as.character(main_df[[cfg$analysis$cluster_var]])
  ww <- as.numeric(w); ww[!is.finite(ww) | ww <= 0] <- NA_real_
  capped <- obs & y > cap
  add <- function(section, metric, stratum = "all", value = NA_real_, text = NA_character_) {
    data.frame(section = section, metric = metric, stratum = stratum,
               value = as.numeric(value), text = as.character(text), stringsAsFactors = FALSE)
  }
  out <- list()
  out[[length(out)+1L]] <- add("cap", "configured_quantile", value = cfg$outcome$continuous_upper_quantile)
  out[[length(out)+1L]] <- add("cap", "cap_dollars", value = cap)
  out[[length(out)+1L]] <- add("cap", "n_observed", value = sum(obs))
  out[[length(out)+1L]] <- add("cap", "n_capped", value = sum(capped))
  out[[length(out)+1L]] <- add("cap", "fraction_capped", value = mean(capped[obs]))
  out[[length(out)+1L]] <- add("cap", "survey_weight_fraction_capped", value = sum(ww[capped], na.rm = TRUE) / sum(ww[obs], na.rm = TRUE))
  out[[length(out)+1L]] <- add("cap", "clusters_with_capped_observation", value = length(unique(cl[capped])))
  for (a in 0:1) {
    ii <- obs & A == a
    out[[length(out)+1L]] <- add("cap_by_arm", "n_observed", paste0("A=",a), sum(ii))
    out[[length(out)+1L]] <- add("cap_by_arm", "n_capped", paste0("A=",a), sum(capped & A == a))
    out[[length(out)+1L]] <- add("cap_by_arm", "survey_weight_fraction_capped", paste0("A=",a),
      sum(ww[capped & A == a], na.rm=TRUE) / sum(ww[ii], na.rm=TRUE))
    raw_total <- sum(ww[ii] * y[ii], na.rm = TRUE)
    ord <- which(ii)[order(y[ii], decreasing = TRUE)]
    for (spec in list(c("top_1_percent", max(1L, ceiling(.01*length(ord)))),
                      c("top_0.5_percent", max(1L, ceiling(.005*length(ord)))),
                      c("top_11", min(11L, length(ord))))) {
      k <- as.integer(spec[[2]]); jj <- utils::head(ord, k)
      out[[length(out)+1L]] <- add("tail_contribution", "fraction_of_weighted_raw_earnings", paste0("A=",a,"_",spec[[1]]),
        if (is.finite(raw_total) && raw_total != 0) sum(ww[jj]*y[jj], na.rm=TRUE)/raw_total else NA_real_)
    }
  }
  for (ss in sort(unique(src))) {
    ii <- capped & src == ss
    out[[length(out)+1L]] <- add("cap_by_source", "n_capped", ss, sum(ii))
  }
  out[[length(out)+1L]] <- add("extremes", "n_at_999995", value = sum(obs & y == 999995))
  out[[length(out)+1L]] <- add("extremes", "n_at_999995_A0", value = sum(obs & y == 999995 & A == 0L))
  out[[length(out)+1L]] <- add("extremes", "n_at_999995_A1", value = sum(obs & y == 999995 & A == 1L))
  out[[length(out)+1L]] <- add("extremes", "top_20_values", text = paste(sort(y[obs], decreasing=TRUE)[seq_len(min(20L,sum(obs)))], collapse=";"))
  do.call(rbind, out)
}

# Dedicated survey-weighted ATT balance table for the 19 fixed Wave I
# Feelings Scale items. It is intentionally not limited by max_balance_variables.
build_protected_h1fs_balance <- function(main_df, cfg, A, survey_w, att_post_w) {
  vars <- cfg$final_tmle$protected_W %||% character(0)
  vars <- vars[vars %in% names(main_df)]
  rows <- lapply(vars, function(v) tryCatch({
    # Use the same frozen complete-Wave-I semantic rule as screening and final
    # nuisance fitting; do not independently recreate H1FS missing codes here.
    rule <- get_missing_rule(main_df[[v]], cfg$preprocessing, variable_name = v)
    masks <- missing_masks_from_rule(main_df[[v]], rule)
    x <- masks$numeric
    x[masks$general | masks$skip] <- NA_real_
    value_row <- balance_one_variable(x, A, survey_w, att_post_w, v)
    miss_row <- balance_one_variable(as.integer(masks$general | masks$skip),
                                     A, survey_w, att_post_w,
                                     paste0(v, "__missing"))
    if (is.null(value_row))
      stop("Protected H1FS balance returned no estimable value contrast.", call. = FALSE)
    value_row$missing_smd_pre <- if (is.null(miss_row)) NA_real_ else miss_row$smd_pre
    value_row$missing_smd_post <- if (is.null(miss_row)) NA_real_ else miss_row$smd_post
    value_row
  }, error = function(e)
    stop(sprintf("Protected H1FS balance failed for '%s': %s", v, conditionMessage(e)),
         call. = FALSE)))
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) return(data.frame())
  out <- do.call(rbind, rows)
  out$comparison <- "A=1_vs_A=0_ATT_protected_H1FS"
  out$abs_smd_pre <- abs(out$smd_pre); out$abs_smd_post <- abs(out$smd_post)
  out$abs_missing_smd_post <- abs(out$missing_smd_post)
  out$exceeds_0_10 <- (is.finite(out$abs_smd_post) & out$abs_smd_post > 0.10) |
    (is.finite(out$abs_missing_smd_post) & out$abs_missing_smd_post > 0.10)
  out <- out[, c("comparison","variable","type","smd_pre","smd_post",
                 "abs_smd_pre","abs_smd_post","missing_smd_pre","missing_smd_post",
                 "abs_missing_smd_post","exceeds_0_10","max_level")]
  out[order(-pmax(out$abs_smd_post, out$abs_missing_smd_post, na.rm = TRUE)), , drop=FALSE]
}

estimate_joint_att_under_clips <- function(tmle_fit, cfg, g_lower, g_upper,
                                           pi_lower, pi_upper, label) {
  required <- c("Qbar1W", "Qbar0W", "QbarAW", "gn_raw", "pi_AW_raw",
                "pi_1W_raw", "pi_0W_raw", "att_components", "weights",
                "respondent_ids", "survey_design_frame", "y_lower", "y_range")
  missing_req <- required[vapply(required, function(nm) is.null(tmle_fit[[nm]]), logical(1))]
  if (length(missing_req))
    stop("Clip sensitivity is missing stored fit component(s): ",
         paste(missing_req, collapse = ", "), call. = FALSE)
  ac <- tmle_fit$att_components
  A <- as.integer(ac$A); delta <- as.integer(ac$delta_Y)
  Y_bounded <- as.numeric(ac$Y_bounded_orig)
  weights <- as.numeric(ac$weights); cluster <- as.character(ac$cluster)
  strata <- if (!is.null(ac$strata)) as.character(ac$strata) else NULL
  w_norm <- weights / mean(weights)
  den_att <- sum(w_norm * A)
  p_treat_w <- den_att / length(weights)
  if (!is.finite(den_att) || den_att <= 0 || !is.finite(p_treat_w) || p_treat_w <= 0)
    stop("Clip sensitivity ATT denominator is invalid.", call. = FALSE)

  clip <- function(p, lo, hi) pmin(pmax(as.numeric(p), lo), hi)
  g <- clip(tmle_fit$gn_raw, g_lower, g_upper)
  pi_AW <- clip(tmle_fit$pi_AW_raw, pi_lower, pi_upper)
  pi_1W <- clip(tmle_fit$pi_1W_raw, pi_lower, pi_upper)
  pi_0W <- clip(tmle_fit$pi_0W_raw, pi_lower, pi_upper)
  if (any(!is.finite(c(g, pi_AW, pi_1W, pi_0W))))
    stop("Clip sensitivity encountered non-finite held-out nuisance predictions.", call. = FALSE)

  Q1 <- as.numeric(tmle_fit$Qbar1W)
  Q0 <- as.numeric(tmle_fit$Qbar0W)
  QAW <- as.numeric(tmle_fit$QbarAW)
  obs <- which(delta == 1L)
  Y_star <- rep(NA_real_, length(Y_bounded))
  Y_star[obs] <- (Y_bounded[obs] - tmle_fit$y_lower) / tmle_fit$y_range

  t1 <- solve_target_score(
    offset = stats::qlogis(QAW[obs]), H = A[obs] / pi_AW[obs],
    Y = Y_star[obs], w = weights[obs],
    score_tol = cfg$final_tmle$target_score_tol %||% 1e-10,
    max_expand = cfg$final_tmle$target_root_max_expand %||% 60L,
    label = paste0("ATT clip sweep mu1: ", label))
  t0 <- solve_target_score(
    offset = stats::qlogis(QAW[obs]),
    H = (1 - A[obs]) * (g[obs] / (1 - g[obs])) / pi_AW[obs],
    Y = Y_star[obs], w = weights[obs],
    score_tol = cfg$final_tmle$target_score_tol %||% 1e-10,
    max_expand = cfg$final_tmle$target_root_max_expand %||% 60L,
    label = paste0("ATT clip sweep mu0: ", label))

  Q1s <- stats::plogis(stats::qlogis(Q1) + t1$epsilon / pi_1W)
  Q0s <- stats::plogis(stats::qlogis(Q0) + t0$epsilon * (g / (1 - g)) / pi_0W)
  Q1o <- Q1s * tmle_fit$y_range + tmle_fit$y_lower
  Q0o <- Q0s * tmle_fit$y_range + tmle_fit$y_lower
  QAo <- ifelse(A == 1L, Q1o, Q0o)
  mu1 <- sum(w_norm * A * Q1o) / den_att
  mu0 <- sum(w_norm * A * Q0o) / den_att
  att <- mu1 - mu0
  D1 <- (1 / p_treat_w) *
    (A * (Q1o - mu1) + A * delta / pi_AW *
       ifelse(delta == 1L, Y_bounded - Q1o, 0))
  D0 <- (1 / p_treat_w) *
    (A * (Q0o - mu0) + (1 - A) * (g / (1 - g)) * delta / pi_AW *
       ifelse(delta == 1L, Y_bounded - Q0o, 0))
  D <- D1 - D0
  inf <- cluster_inference_from_eif(
    D, weights, cluster, att, strata = strata,
    design_frame = tmle_fit$survey_design_frame,
    domain_ids = tmle_fit$respondent_ids,
    id_var = cfg$analysis$id_var, cluster_var = cfg$analysis$cluster_var,
    strata_var = cfg$analysis$strata_var, weight_var = cfg$analysis$weight_var)

  report_ratio <- compensation_ratio_translation_enabled(cfg)
  pct_prev <- pct_prev_se <- pct_prev_lo <- pct_prev_hi <- pct_prev_p <- NA_real_
  if (report_ratio) {
    if (!is.finite(mu1) || !is.finite(mu0) || mu1 <= 0 || mu0 <= 0)
      stop("Clip sensitivity ratio translation requires positive targeted component means.", call. = FALSE)
    pct_prev <- 100 * (mu0 - mu1) / mu1
    D_pct_prev <- 100 * (D0 / mu1 - mu0 * D1 / mu1^2)
    inf_pct <- cluster_inference_from_eif(
      D_pct_prev, weights, cluster, pct_prev, strata = strata,
      design_frame = tmle_fit$survey_design_frame,
      domain_ids = tmle_fit$respondent_ids,
      id_var = cfg$analysis$id_var, cluster_var = cfg$analysis$cluster_var,
      strata_var = cfg$analysis$strata_var, weight_var = cfg$analysis$weight_var)
    pct_prev_se <- inf_pct$se; pct_prev_lo <- inf_pct$ci[1L]
    pct_prev_hi <- inf_pct$ci[2L]; pct_prev_p <- inf_pct$p
  }

  controls <- A == 0L
  ctrl_odds <- g[controls] / (1 - g[controls])
  ctrl_survey_odds <- weights[controls] * ctrl_odds
  ctrl_ipcw <- ctrl_survey_odds / pi_0W[controls]
  data.frame(
    scenario = label,
    compensation_transform = if (identical(cfg$outcome$family, "Compensation"))
      tolower(cfg$outcome$compensation_transform %||% "identity") else NA_character_,
    prevention_gain_defined = report_ratio,
    g_lower = g_lower, g_upper = g_upper,
    pi_lower = pi_lower, pi_upper = pi_upper,
    att_estimate = att, att_se = inf$se,
    att_ci_lower = inf$ci[1L], att_ci_upper = inf$ci[2L], att_p = inf$p,
    mu1 = mu1, mu0 = mu0,
    prevention_gain_pct = pct_prev,
    prevention_gain_se = pct_prev_se,
    prevention_gain_ci_lower = pct_prev_lo,
    prevention_gain_ci_upper = pct_prev_hi,
    prevention_gain_p = pct_prev_p,
    target_score_mu1 = t1$normalized_score,
    target_score_mu0 = t0$normalized_score,
    scaled_eif_mean = (sum(w_norm * D) / length(weights)) / tmle_fit$y_range,
    frac_g_clipped = mean(abs(g - tmle_fit$gn_raw) > 1e-12),
    frac_pi_AW_clipped = mean(abs(pi_AW - tmle_fit$pi_AW_raw) > 1e-12),
    frac_pi1_clipped = mean(abs(pi_1W - tmle_fit$pi_1W_raw) > 1e-12),
    frac_pi0_clipped = mean(abs(pi_0W - tmle_fit$pi_0W_raw) > 1e-12),
    att_control_odds_ess = kish_eff_n(ctrl_odds),
    att_control_odds_p99 = if (length(ctrl_odds)) as.numeric(stats::quantile(ctrl_odds, 0.99, names = FALSE, type = 8)) else NA_real_,
    att_control_odds_max = if (length(ctrl_odds)) max(ctrl_odds) else NA_real_,
    att_control_survey_weight_ess = kish_eff_n(ctrl_survey_odds),
    att_control_ipcw_weight_ess = kish_eff_n(ctrl_ipcw),
    att_control_ipcw_weight_p99 = if (length(ctrl_ipcw)) as.numeric(stats::quantile(ctrl_ipcw, 0.99, names = FALSE, type = 8)) else NA_real_,
    att_control_ipcw_weight_max = if (length(ctrl_ipcw)) max(ctrl_ipcw) else NA_real_,
    stringsAsFactors = FALSE)
}

build_att_mnar_pattern_mixture <- function(tmle_fit, cfg) {
  ac <- tmle_fit$att_components
  if (is.null(ac)) return(data.frame())
  needed <- c("A", "weights", "delta_Y", "pi_AW", "pi_1W", "pi_0W",
              "Y_bounded_orig", "Qbar1W_orig", "Qbar0W_orig", "cluster",
              "psi_att", "att_se", "y_lower", "y_upper", "inference_df")
  if (!all(needed %in% names(ac)))
    stop("MNAR sensitivity is missing required ATT components.", call. = FALSE)
  A <- as.integer(ac$A); w <- as.numeric(ac$weights)
  w_norm <- w / mean(w)
  den_att <- sum(w_norm * A)
  if (!is.finite(den_att) || den_att <= 0)
    stop("MNAR sensitivity has no positive treated mass.", call. = FALSE)
  obs <- ac$delta_Y == 1L & is.finite(ac$Y_bounded_orig)
  ipcw_w <- w[obs] / ac$pi_AW[obs]
  outcome_sd <- sqrt(weighted_var_safe(ac$Y_bounded_orig[obs], ipcw_w))
  if (!is.finite(outcome_sd) || outcome_sd <= 0)
    stop("MNAR sensitivity could not estimate a positive canonical-outcome SD.", call. = FALSE)
  grid <- sort(unique(as.numeric(cfg$diagnostics$mnar_shift_sd_grid %||%
                                   c(-0.5, -0.25, 0, 0.25, 0.5))))
  pairs <- expand.grid(delta1_sd = grid, delta0_sd = grid,
                       KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  q1 <- as.numeric(ac$Qbar1W_orig); q0 <- as.numeric(ac$Qbar0W_orig)
  out <- lapply(seq_len(nrow(pairs)), function(i) {
    d1 <- pairs$delta1_sd[i] * outcome_sd
    d0 <- pairs$delta0_sd[i] * outcome_sd
    q1_miss <- pmin(pmax(q1 + d1, ac$y_lower), ac$y_upper)
    q0_miss <- pmin(pmax(q0 + d0, ac$y_lower), ac$y_upper)
    shift1 <- sum(w_norm * A * (1 - ac$pi_1W) * (q1_miss - q1)) / den_att
    shift0 <- sum(w_norm * A * (1 - ac$pi_0W) * (q0_miss - q0)) / den_att
    net_shift <- shift1 - shift0
    data.frame(
      delta1_sd = pairs$delta1_sd[i],
      delta0_sd = pairs$delta0_sd[i],
      delta1_outcome_units = d1,
      delta0_outcome_units = d0,
      outcome_sd_used = outcome_sd,
      mu1_shift = shift1,
      mu0_shift = shift0,
      att_net_shift = net_shift,
      adjusted_att = ac$psi_att + net_shift,
      adjusted_ci_lower_conditional = ac$psi_att + net_shift -
        stats::qt(0.975, df = max(1, ac$inference_df)) * ac$att_se,
      adjusted_ci_upper_conditional = ac$psi_att + net_shift +
        stats::qt(0.975, df = max(1, ac$inference_df)) * ac$att_se,
      q1_support_clip_fraction = weighted_mean_safe(
        as.numeric(q1 + d1 < ac$y_lower | q1 + d1 > ac$y_upper),
        w_norm * A * (1 - ac$pi_1W)),
      q0_support_clip_fraction = weighted_mean_safe(
        as.numeric(q0 + d0 < ac$y_lower | q0 + d0 > ac$y_upper),
        w_norm * A * (1 - ac$pi_0W)),
      interpretation = paste0(
        "Fixed-nuisance pattern-mixture shift: missing potential outcomes under A=1/A=0 ",
        "are shifted from respondent conditional means by delta1/delta0, bounded to the outcome support."),
      stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# ---------------------------------------------------------------------------
# MNAR sensitivity extensions (v8.28): shared fixed-nuisance machinery.
# ---------------------------------------------------------------------------
# Extracts the pieces of the TARGETED ATT fit needed to re-evaluate the point
# estimate under alternative assumptions about the conditional mean of the
# outcome among non-responders.  Nothing here refits a nuisance model, changes
# the estimand, or touches the primary ATT: every downstream diagnostic is a
# deterministic function of quantities already produced by run_final_cv_tmle().
#
# All three new diagnostics route through this helper and through
# mnar_net_shift_from_means() so that they are numerically consistent with
# att_mnar_pattern_mixture.csv by construction (identical arithmetic; only the
# choice of the non-responder conditional means differs).
mnar_fixed_nuisance_components <- function(ac) {
  needed <- c("A", "weights", "delta_Y", "pi_AW", "pi_1W", "pi_0W",
              "Y_bounded_orig", "Qbar1W_orig", "Qbar0W_orig",
              "psi_att", "att_se", "y_lower", "y_upper", "inference_df")
  if (!all(needed %in% names(ac)))
    stop("MNAR fixed-nuisance components are missing required ATT fields: ",
         paste(setdiff(needed, names(ac)), collapse = ", "), call. = FALSE)
  A <- as.integer(ac$A)
  w <- as.numeric(ac$weights)
  # Matches build_att_mnar_pattern_mixture() exactly.
  w_norm <- w / mean(w)
  den_att <- sum(w_norm * A)
  if (!is.finite(den_att) || den_att <= 0)
    stop("MNAR fixed-nuisance components have no positive treated mass.", call. = FALSE)
  obs <- ac$delta_Y == 1L & is.finite(ac$Y_bounded_orig)
  if (!any(obs))
    stop("MNAR fixed-nuisance components have no observed outcomes.", call. = FALSE)
  ipcw_w <- w[obs] / ac$pi_AW[obs]
  outcome_sd <- sqrt(weighted_var_safe(ac$Y_bounded_orig[obs], ipcw_w))
  if (!is.finite(outcome_sd) || outcome_sd <= 0)
    stop("MNAR fixed-nuisance components could not estimate a positive outcome SD.",
         call. = FALSE)
  pi_1W <- as.numeric(ac$pi_1W); pi_0W <- as.numeric(ac$pi_0W)
  # Fraction of the ATT's treated mass whose potential outcome is unobserved
  # under each intervention arm.  These are the multipliers that convert a
  # per-person shift into a shift in the estimate.
  lev1 <- sum(w_norm * A * (1 - pi_1W)) / den_att
  lev0 <- sum(w_norm * A * (1 - pi_0W)) / den_att
  list(
    A = A, weights = w, w_norm = w_norm, den_att = den_att,
    q1 = as.numeric(ac$Qbar1W_orig), q0 = as.numeric(ac$Qbar0W_orig),
    pi_1W = pi_1W, pi_0W = pi_0W, pi_AW = as.numeric(ac$pi_AW),
    y_lower = as.numeric(ac$y_lower), y_upper = as.numeric(ac$y_upper),
    outcome_sd = outcome_sd, obs = obs,
    Y_obs = as.numeric(ac$Y_bounded_orig),
    psi_att = as.numeric(ac$psi_att), att_se = as.numeric(ac$att_se),
    tcrit = stats::qt(0.975, df = max(1, ac$inference_df)),
    leverage_treated_arm = lev1, leverage_control_arm = lev0)
}

# Net change in the ATT when the conditional means among NON-RESPONDERS are
# replaced by q1_miss (under A=1) and q0_miss (under A=0), bounded to the
# outcome support.  Identical arithmetic to build_att_mnar_pattern_mixture().
mnar_net_shift_from_means <- function(cmp, q1_miss, q0_miss) {
  q1_miss <- pmin(pmax(q1_miss, cmp$y_lower), cmp$y_upper)
  q0_miss <- pmin(pmax(q0_miss, cmp$y_lower), cmp$y_upper)
  shift1 <- sum(cmp$w_norm * cmp$A * (1 - cmp$pi_1W) * (q1_miss - cmp$q1)) / cmp$den_att
  shift0 <- sum(cmp$w_norm * cmp$A * (1 - cmp$pi_0W) * (q0_miss - cmp$q0)) / cmp$den_att
  c(shift1 = shift1, shift0 = shift0, net = shift1 - shift0)
}

# Convenience wrapper for additive shifts expressed in outcome units.
mnar_net_shift_from_deltas <- function(cmp, d1, d0) {
  mnar_net_shift_from_means(cmp, cmp$q1 + d1, cmp$q0 + d0)
}

# ---------------------------------------------------------------------------
# (1) Breakdown point / tipping point for the MNAR assumption.
# ---------------------------------------------------------------------------
# Reports the SMALLEST departure from MAR -- in outcome SD units and in outcome
# units -- at which the primary 95% interval first admits the null, for several
# pre-specified directions of departure.  This is the conventional way to report
# a delta-adjustment sensitivity analysis (NRC 2010; Masten & Poirier 2020) and
# replaces reading a threshold off the pattern-mixture grid by eye.
build_att_mnar_breakdown <- function(tmle_fit, cfg) {
  if (is.null(tmle_fit) || is.null(tmle_fit$att_components)) return(data.frame())
  ac <- tmle_fit$att_components
  cmp <- mnar_fixed_nuisance_components(ac)
  ci_lo0 <- cmp$psi_att - cmp$tcrit * cmp$att_se
  ci_hi0 <- cmp$psi_att + cmp$tcrit * cmp$att_se
  if (!is.finite(ci_lo0) || !is.finite(ci_hi0))
    stop("MNAR breakdown requires a finite primary confidence interval.", call. = FALSE)
  max_sd  <- as.numeric(cfg$diagnostics$mnar_breakdown_max_sd %||% 3)
  grid_n  <- as.integer(cfg$diagnostics$mnar_breakdown_grid_n %||% 601L)
  if (!is.finite(max_sd) || max_sd <= 0) stop("mnar_breakdown_max_sd must be positive.", call. = FALSE)
  if (!is.finite(grid_n) || grid_n < 11L) stop("mnar_breakdown_grid_n must be >= 11.", call. = FALSE)

  base_row <- function(nm, u1, u0, s, note) {
    net <- if (is.na(s)) NA_real_ else
      unname(mnar_net_shift_from_deltas(cmp, s * u1 * cmp$outcome_sd,
                                        s * u0 * cmp$outcome_sd)["net"])
    data.frame(
      direction = nm, delta1_multiplier = u1, delta0_multiplier = u0,
      breakdown_delta_sd_signed = s,
      breakdown_delta_sd_abs = if (is.na(s)) NA_real_ else abs(s),
      breakdown_delta_outcome_units = if (is.na(s)) NA_real_ else abs(s) * cmp$outcome_sd,
      net_shift_at_breakdown = net,
      adjusted_att_at_breakdown = if (is.na(net)) NA_real_ else cmp$psi_att + net,
      primary_att = cmp$psi_att, primary_ci_lower = ci_lo0, primary_ci_upper = ci_hi0,
      outcome_sd_used = cmp$outcome_sd,
      leverage_treated_arm = cmp$leverage_treated_arm,
      leverage_control_arm = cmp$leverage_control_arm,
      searched_max_sd = max_sd,
      fixed_nuisance = TRUE,
      primary_se_held_fixed = TRUE,
      configured_outcome_cap_held_fixed = TRUE,
      note = note, stringsAsFactors = FALSE)
  }

  # If the primary interval already admits the null there is no breakdown point.
  if (ci_lo0 <= 0 && ci_hi0 >= 0)
    return(base_row("not_applicable", NA_real_, NA_real_, NA_real_,
                    "Primary interval already includes the null; breakdown point undefined."))

  # margin(s) < 0  <=>  interval still excludes the null at signed shift s.
  margin_at <- if (ci_hi0 < 0) {
    function(s, u1, u0) cmp$psi_att +
      unname(mnar_net_shift_from_deltas(cmp, s * u1 * cmp$outcome_sd,
                                        s * u0 * cmp$outcome_sd)["net"]) +
      cmp$tcrit * cmp$att_se
  } else {
    function(s, u1, u0) -(cmp$psi_att +
      unname(mnar_net_shift_from_deltas(cmp, s * u1 * cmp$outcome_sd,
                                        s * u0 * cmp$outcome_sd)["net"]) -
      cmp$tcrit * cmp$att_se)
  }

  scan_direction <- function(u1, u0) {
    grid <- seq(0, max_sd, length.out = grid_n)
    best <- NA_real_
    for (sgn in c(1, -1)) {
      vals <- vapply(grid, function(s) margin_at(sgn * s, u1, u0), numeric(1))
      hit <- which(is.finite(vals) & vals >= 0)
      if (!length(hit)) next
      k <- hit[1L]
      cand <- if (k == 1L) 0 else tryCatch(
        stats::uniroot(function(s) margin_at(sgn * s, u1, u0),
                       lower = grid[k - 1L], upper = grid[k])$root,
        error = function(e) grid[k])
      cand <- sgn * cand
      if (is.na(best) || abs(cand) < abs(best)) best <- cand
    }
    best
  }

  dirs <- list(
    list(nm = "treated_arm_only",       u1 = 1,    u0 = 0),
    list(nm = "control_arm_only",       u1 = 0,    u0 = 1),
    list(nm = "opposing_arms_separation", u1 = 0.5, u0 = -0.5),
    list(nm = "common_shift_both_arms", u1 = 1,    u0 = 1))
  rows <- lapply(dirs, function(d) {
    s <- scan_direction(d$u1, d$u0)
    note <- if (is.na(s))
      sprintf("No breakdown within +/- %.2f SD in this direction.", max_sd)
    else
      "Smallest departure from MAR at which the 95% interval first admits the null."
    base_row(d$nm, d$u1, d$u0, s, note)
  })
  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
# (2) Fixed-nuisance extreme-mean sensitivity for the SAME bounded ATT.
# ---------------------------------------------------------------------------
# Replaces the non-responder conditional means with the extremes of the
# configured bounded outcome support while holding the fitted nuisance functions,
# empirical cap, and primary standard error fixed. These are deliberately broad
# orientation bounds for this post-hoc sensitivity calculation; they are not
# assumption-free bounds for uncapped earnings and are not confidence intervals.
build_att_manski_bounds <- function(tmle_fit, cfg) {
  if (is.null(tmle_fit) || is.null(tmle_fit$att_components)) return(data.frame())
  cmp <- mnar_fixed_nuisance_components(tmle_fit$att_components)
  n <- length(cmp$q1)
  lo <- mnar_net_shift_from_means(cmp, rep(cmp$y_lower, n), rep(cmp$y_upper, n))
  hi <- mnar_net_shift_from_means(cmp, rep(cmp$y_upper, n), rep(cmp$y_lower, n))
  att_lo <- cmp$psi_att + unname(lo["net"])
  att_hi <- cmp$psi_att + unname(hi["net"])
  if (att_lo > att_hi)
    stop("Fixed-nuisance extreme-mean bounds are inverted; check outcome support.",
         call. = FALSE)
  data.frame(
    estimand = "ATT fixed-nuisance sensitivity for configured bounded outcome",
    fixed_nuisance = TRUE,
    configured_bounded_outcome = TRUE,
    y_lower = cmp$y_lower, y_upper = cmp$y_upper,
    outcome_sd_used = cmp$outcome_sd,
    leverage_treated_arm = cmp$leverage_treated_arm,
    leverage_control_arm = cmp$leverage_control_arm,
    primary_att = cmp$psi_att,
    att_extreme_mean_lower = att_lo,
    att_extreme_mean_upper = att_hi,
    bound_width = att_hi - att_lo,
    bounds_exclude_null = (att_lo > 0) || (att_hi < 0),
    conservative_orientation_band_lower = att_lo - cmp$tcrit * cmp$att_se,
    conservative_orientation_band_upper = att_hi + cmp$tcrit * cmp$att_se,
    interpretation = paste0(
      "Fixed-nuisance extreme-mean sensitivity for the configured bounded outcome: ",
      "non-responder conditional means are set to support extremes while fitted nuisances, ",
      "the empirical cap, and the primary SE are held fixed. The orientation band is not ",
      "a confidence interval and these are not assumption-free bounds for uncapped earnings."),
    stringsAsFactors = FALSE)
}


# ---------------------------------------------------------------------------
# (3) Calibrated MNAR sensitivity model.
# ---------------------------------------------------------------------------
# Follows McClean, Branson & Kennedy (2024) in expressing the sensitivity
# parameter as a RATIO of unmeasured to measured bias rather than in raw units,
# and in carrying uncertainty in the measured-bias estimate through to the
# reported parameter.  Adapted here from unmeasured confounding to MNAR:
#
#   U <= Gamma * M,  U = required per-person shift among non-responders,
#                    M = OBSERVED outcome gradient across response-propensity
#                        strata among responders.
#
# M is estimated as the survey-weighted difference in observed outcomes between
# the top and bottom response-propensity strata (cut points from
# mnar_calibration_probs), with a PSU-level cluster bootstrap for uncertainty.
# Gamma is then the breakdown shift divided by M.  Gamma > 1 means the MNAR
# departure required to overturn the result exceeds the missingness-related
# outcome gradient that is actually visible in the observed data.
#
# NOTE: Gamma is interpretable ONLY relative to this definition of M. Pre-declare
# mnar_calibration_probs; do not tune it after seeing Gamma.
weighted_hf8_quantiles <- function(x, w, probs) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  if (!any(keep)) return(rep(NA_real_, length(probs)))
  dat <- data.frame(x = as.numeric(x[keep]), w = as.numeric(w[keep]))
  des <- survey::svydesign(ids = ~1, weights = ~w, data = dat)
  q <- survey::svyquantile(~x, design = des, quantiles = probs,
                           qrule = "hf8", ci = FALSE, se = FALSE, na.rm = TRUE)
  as.numeric(stats::coef(q))
}

build_att_mnar_calibrated <- function(tmle_fit, cfg) {
  if (is.null(tmle_fit) || is.null(tmle_fit$att_components)) return(data.frame())
  ac <- tmle_fit$att_components
  if (is.null(ac$cluster) || is.null(ac$strata))
    stop("Calibrated MNAR sensitivity requires cluster and REGION identifiers.",
         call. = FALSE)
  cmp <- mnar_fixed_nuisance_components(ac)
  bd <- build_att_mnar_breakdown(tmle_fit, cfg)
  if (!nrow(bd) || all(is.na(bd$breakdown_delta_sd_abs))) return(data.frame())

  obs <- cmp$obs
  pv <- cmp$pi_AW[obs]; yv <- cmp$Y_obs[obs]; wv <- cmp$weights[obs]
  cl <- as.character(ac$cluster)[obs]
  st <- as.character(ac$strata)[obs]
  keep <- is.finite(pv) & is.finite(yv) & is.finite(wv) & wv > 0 &
    !is.na(cl) & nzchar(cl) & !is.na(st) & nzchar(st)
  pv <- pv[keep]; yv <- yv[keep]; wv <- wv[keep]; cl <- cl[keep]; st <- st[keep]
  if (length(pv) < 50L)
    stop("Calibrated MNAR sensitivity has too few observed rows.", call. = FALSE)
  probs <- as.numeric(cfg$diagnostics$mnar_calibration_probs)

  psu_map <- unique(data.frame(cluster = cl, stratum = st, stringsAsFactors = FALSE))
  if (anyDuplicated(psu_map$cluster))
    stop("A PSU maps to more than one REGION in the calibrated MNAR sample.",
         call. = FALSE)

  measured_gap <- function(p, y, w) {
    qs <- weighted_hf8_quantiles(p, w, probs)
    if (!all(is.finite(qs)) || qs[1L] >= qs[2L]) return(NA_real_)
    lo_i <- p <= qs[1L]; hi_i <- p >= qs[2L]
    if (!any(lo_i) || !any(hi_i)) return(NA_real_)
    weighted_mean_safe(y[hi_i], w[hi_i]) -
      weighted_mean_safe(y[lo_i], w[lo_i])
  }
  M_signed <- measured_gap(pv, yv, wv)
  if (!is.finite(M_signed) || abs(M_signed) <= 0)
    stop("Calibrated MNAR sensitivity could not estimate a non-zero measured gradient.",
         call. = FALSE)
  M_dollars <- abs(M_signed)
  M_sd <- M_dollars / cmp$outcome_sd

  B <- as.integer(cfg$diagnostics$mnar_calibration_boot_reps)
  seed <- as.integer(cfg$diagnostics$mnar_calibration_boot_seed)
  min_valid <- as.integer(cfg$diagnostics$mnar_calibration_min_valid_boot_reps)
  psus_by_stratum <- split(psu_map$cluster, psu_map$stratum)
  too_small <- names(psus_by_stratum)[vapply(psus_by_stratum, length, integer(1)) < 2L]
  if (length(too_small))
    stop("Calibrated MNAR bootstrap requires at least two PSUs in every REGION stratum; deficient strata: ",
         paste(too_small, collapse = ", "), call. = FALSE)
  boot_signed <- with_local_seed(seed, vapply(seq_len(B), function(b) {
    multiplicity <- setNames(integer(nrow(psu_map)), psu_map$cluster)
    for (ss in names(psus_by_stratum)) {
      ids <- psus_by_stratum[[ss]]
      picked <- sample(ids, length(ids), replace = TRUE)
      tab <- table(picked)
      multiplicity[names(tab)] <- as.integer(tab)
    }
    wb <- wv * unname(multiplicity[cl])
    measured_gap(pv, yv, wb)
  }, numeric(1)))
  valid_boot <- is.finite(boot_signed) & boot_signed != 0
  boot_abs <- abs(boot_signed[valid_boot])
  if (length(boot_abs) < min_valid)
    stop(sprintf(
      "Calibrated MNAR bootstrap produced only %d valid replicates; %d are required.",
      length(boot_abs), min_valid), call. = FALSE)
  M_lo <- unname(stats::quantile(boot_abs, 0.025, type = 8))
  M_hi <- unname(stats::quantile(boot_abs, 0.975, type = 8))
  sign_reversal_fraction <- mean(sign(boot_signed[valid_boot]) != sign(M_signed))

  out <- lapply(seq_len(nrow(bd)), function(i) {
    s_abs <- bd$breakdown_delta_sd_abs[i]
    gam <- if (is.na(s_abs)) NA_real_ else s_abs / M_sd
    gam_lo <- if (is.na(s_abs)) NA_real_ else s_abs / (M_hi / cmp$outcome_sd)
    gam_hi <- if (is.na(s_abs)) NA_real_ else s_abs / (M_lo / cmp$outcome_sd)
    data.frame(
      direction = bd$direction[i],
      breakdown_delta_sd_abs = s_abs,
      breakdown_delta_outcome_units = bd$breakdown_delta_outcome_units[i],
      measured_gradient_outcome_units = M_dollars,
      measured_gradient_sd = M_sd,
      measured_gradient_signed = M_signed,
      measured_gradient_boot_lower = M_lo,
      measured_gradient_boot_upper = M_hi,
      measured_gradient_boot_reps_requested = B,
      measured_gradient_boot_reps_used = length(boot_abs),
      measured_gradient_boot_sign_reversal_fraction = sign_reversal_fraction,
      bootstrap_design = paste0(
        "Approximate REGION-stratified PSU cluster bootstrap; PSUs resampled ",
        "with replacement within REGION and multiplicities carried into survey weights"),
      propensity_cutpoint_rule = "survey-weighted Hyndman-Fan type 8",
      calibrated_gamma = gam,
      calibrated_gamma_lower = gam_lo,
      calibrated_gamma_upper = gam_hi,
      gamma_interval_conditions = paste0(
        "Propagates uncertainty in the measured gradient only; breakdown shift, ",
        "fitted nuisances, configured outcome cap, and primary SE are held fixed."),
      response_propensity_prob_low = probs[1L],
      response_propensity_prob_high = probs[2L],
      outcome_sd_used = cmp$outcome_sd,
      interpretation = paste0(
        "Descriptive calibrated MNAR sensitivity adapted from a confounding framework. ",
        "Gamma is interpretable only relative to the prespecified weighted propensity strata."),
      stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}


build_att_g_pi_clip_sensitivity <- function(tmle_fit, cfg) {
  if (is.null(tmle_fit) || is.null(tmle_fit$att_components)) return(data.frame())
  rows <- list()
  if (isTRUE(cfg$diagnostics$att_g_pi_clip_include_configured %||% TRUE)) {
    rows[[length(rows) + 1L]] <- estimate_joint_att_under_clips(
      tmle_fit, cfg,
      cfg$final_tmle$g_lower, cfg$final_tmle$g_upper,
      cfg$final_tmle$pi_lower, cfg$final_tmle$pi_upper,
      "configured_primary_bounds")
  }
  floors <- sort(unique(as.numeric(cfg$diagnostics$att_g_pi_clip_sensitivity_floors %||%
                                     c(0.01, 0.025, 0.05))))
  for (f in floors) {
    rows[[length(rows) + 1L]] <- estimate_joint_att_under_clips(
      tmle_fit, cfg, f, 1 - f, f, 1 - f,
      sprintf("symmetric_floor_%.3f", f))
  }
  out <- do.call(rbind, rows)
  # This sweep re-targets the joint ATT TMLE, so reconcile to the stored TMLE
  # component even if a separate sensitivity run chooses the one-step ATT as
  # its displayed headline.
  reference_att <- tmle_fit$att_components$att_tmle_estimate %||%
    tmle_fit$att_components$psi_att
  if (is.null(reference_att) || length(reference_att) != 1L || !is.finite(reference_att))
    stop("Clip sensitivity could not find a finite stored ATT TMLE reference.", call. = FALSE)
  out$difference_from_primary_tmle <- out$att_estimate - reference_att
  hit <- which(out$scenario == "configured_primary_bounds")
  if (length(hit) == 1L &&
      (!is.finite(out$difference_from_primary_tmle[hit]) ||
       abs(out$difference_from_primary_tmle[hit]) > 1e-6)) {
    stop(sprintf("Configured clip-sweep row failed to reproduce the primary ATT TMLE (difference %.6g).",
                 out$difference_from_primary_tmle[hit]), call. = FALSE)
  }
  out
}

run_expanded_wave2_completion_diagnostic <- function(cfg, main_df, tmle_fit, w1_all) {
  status <- attr(main_df, "wave2_completion_status", exact = TRUE)
  if (is.null(status) || !is.data.frame(status) || is.null(w1_all) || !is.data.frame(w1_all)) return(NULL)
  idv <- cfg$analysis$id_var; clv <- cfg$analysis$cluster_var; wv <- cfg$analysis$weight_var
  assert_required_columns(status, c(idv, "wave2_cesd_complete"), "Wave-II completion status")
  assert_required_columns(w1_all, c(idv, clv, wv), "Wave-I completion diagnostic source")
  m <- match(as.character(w1_all[[idv]]), as.character(status[[idv]])); keep <- !is.na(m)
  d <- w1_all[keep, , drop = FALSE]; complete <- as.integer(status$wave2_cesd_complete[m[keep]])
  if (anyNA(complete) || any(!complete %in% c(0L,1L))) stop("Invalid Wave-II completion indicator.", call.=FALSE)
  selected <- unique(c(cfg$final_tmle$protected_W %||% character(0), unlist(tmle_fit$selected_by_fold %||% list(), use.names = FALSE)))
  selected <- intersect(selected, get_candidate_vars(d, cfg))
  if (!length(selected)) {
    message("WARNING: Expanded Wave-II completion diagnostic found no protected or ever-selected Wave-I variables.")
    return(NULL)
  }
  w <- suppressWarnings(as.numeric(d[[wv]])); ok <- is.finite(w) & w > 0 & !is.na(d[[clv]])
  d <- d[ok, , drop=FALSE]; complete <- complete[ok]; w <- w[ok]
  bal <- make_balance_table(d, cfg, complete, w, "Wave2_CESD_complete_vs_incomplete_expanded", priority_vars = selected)
  pred <- rep(NA_real_, nrow(d))
  folds <- make_cluster_folds_balanced(as.character(d[[clv]]), complete, k=5L, seed=seed_for(cfg,808080L),
    weights=w, delta=rep(1L,nrow(d)), balance_on_weights=isTRUE(cfg$final_tmle$internal_fold_balance_on_weights), max_attempts=500L)
  for (v in sort(unique(folds))) {
    tr <- which(folds != v); te <- which(folds == v)
    raw_tr <- d[tr, selected, drop=FALSE]; raw_te <- d[te, selected, drop=FALSE]
    mr <- learn_final_missing_recipe(raw_tr, cfg$preprocessing)
    ptr <- apply_final_missing_recipe(raw_tr, mr, cfg$preprocessing); pte <- apply_final_missing_recipe(raw_te, mr, cfg$preprocessing)
    des <- build_grouped_design_train(ptr, cfg$final_preprocess, cfg$preprocessing,
      hard_max_cols=max(2000L, cfg$final_tmle$hard_max_processed_columns %||% 450L), A=complete[tr],
      protected_raw_vars=cfg$final_tmle$protected_W %||% character(0))
    xtr <- des$X; xte <- apply_preprocess_recipe(pte, des$recipes)
    miss <- setdiff(colnames(xtr), colnames(xte)); extra <- setdiff(colnames(xte), colnames(xtr))
    if (length(miss)) xte <- cbind(xte, matrix(0,nrow(xte),length(miss),dimnames=list(NULL,miss)))
    if (length(extra)) xte <- xte[,!colnames(xte)%in%extra,drop=FALSE]
    xte <- xte[,colnames(xtr),drop=FALSE]
    intfold <- make_cluster_folds_balanced(as.character(d[[clv]][tr]), complete[tr], k=3L, seed=seed_for(cfg,808080L+v),
      weights=w[tr], delta=rep(1L,length(tr)), balance_on_weights=isTRUE(cfg$final_tmle$internal_fold_balance_on_weights), max_attempts=500L)
    fit <- glmnet::cv.glmnet(x=if(requireNamespace("Matrix",quietly=TRUE)) Matrix::Matrix(xtr,sparse=TRUE) else xtr,
      y=complete[tr], family="binomial", alpha=cfg$learners$glmnet$alpha %||% 0.5,
      nlambda=cfg$learners$glmnet$nlambda %||% 100L, weights=w[tr], foldid=intfold,
      standardize=isTRUE(cfg$learners$glmnet$standardize %||% TRUE), maxit=cfg$learners$glmnet$maxit %||% 100000L)
    pred[te] <- as.numeric(predict(fit,newx=xte,s=fit$lambda.min,type="response"))
  }
  if (any(!is.finite(pred))) stop("Wave-II completion model produced invalid held-out predictions.",call.=FALSE)
  wn <- normalize_positive_weights(w,length(w),"Wave-II completion model weights"); pc <- pmin(pmax(pred,1e-12),1-1e-12)
  model <- data.frame(n=nrow(d), n_complete=sum(complete==1L), n_incomplete=sum(complete==0L),
    weighted_completion_rate=sum(wn*complete)/sum(wn), cross_fitted_weighted_auc=weighted_auc_tie_corrected(complete,pred,wn),
    cross_fitted_weighted_brier=sum(wn*(complete-pred)^2)/sum(wn),
    cross_fitted_weighted_logloss=-sum(wn*(complete*log(pc)+(1-complete)*log(1-pc)))/sum(wn),
    prediction_min=min(pred), prediction_p01=as.numeric(quantile(pred,.01,names=FALSE,type=8)), prediction_median=median(pred),
    prediction_p99=as.numeric(quantile(pred,.99,names=FALSE,type=8)), prediction_max=max(pred), n_predictors_raw=length(selected), stringsAsFactors=FALSE)
  list(balance=bal, model=model)
}

run_peer_review_diagnostics <- function(cfg, main_df, prescreen_results = NULL,
                                        tmle_fit = NULL, raw_w1_rows = NULL,
                                        w1_all = NULL) {
  if (is.null(cfg$preprocessing$global_missing_dictionary) ||
      !length(cfg$preprocessing$global_missing_dictionary)) {
    cfg$preprocessing$global_missing_dictionary <-
      attr(main_df, "global_missing_dictionary", exact = TRUE)
  }
  if (isTRUE(cfg$preprocessing$global_missing_dictionary_required %||% TRUE) &&
      (is.null(cfg$preprocessing$global_missing_dictionary) ||
       !length(cfg$preprocessing$global_missing_dictionary))) {
    stop("Diagnostics require the frozen complete-Wave-I missing-code dictionary.",
         call. = FALSE)
  }
  if (!isTRUE(cfg$diagnostics$enable)) return(invisible(NULL))
  msg("\n===== STAGE: Peer-review diagnostics =====", cfg = cfg)
  out_dir <- file.path(cfg$global$output_dir, cfg$diagnostics$diagnostics_dir)
  ensure_output_dir(out_dir)
  msg(sprintf("  [diag] Output directory: %s", out_dir), cfg = cfg)

  optional_status_rows <- list()
  balance_scan_timing_rows <- list()
  run_optional <- function(label, fun) {
    z <- capture_optional_diagnostic(label, fun)
    optional_status_rows[[length(optional_status_rows) + 1L]] <<- z$status
    z$value
  }
  record_balance_scan <- function(x, label, n_rows) {
    sec <- attr(x, "balance_scan_seconds", exact = TRUE)
    nc <- attr(x, "balance_scan_n_candidates", exact = TRUE)
    if (is.null(sec) || !is.finite(sec)) return(invisible(NULL))
    balance_scan_timing_rows[[length(balance_scan_timing_rows) + 1L]] <<-
      data.frame(
        comparison = as.character(label), n_rows = as.integer(n_rows),
        n_candidates = as.integer(nc %||% NA_integer_), seconds = as.numeric(sec),
        stringsAsFactors = FALSE)
    invisible(NULL)
  }

  A <- normalize_binary_var(main_df[[cfg$analysis$exposure_var]], cfg$analysis$exposure_var)
  delta_Y <- as.integer(!is.na(main_df[[cfg$analysis$outcome_var]]))
  w <- as.numeric(main_df[[cfg$analysis$weight_var]])
  w[!is.finite(w) | w <= 0] <- NA_real_

  # Targeted missing-code audit and raw-dollar tail/cap audit are descriptive
  # safeguards only; neither changes the analysis data or estimator.
  miss_audit <- build_targeted_missing_code_audit(main_df, cfg, tmle_fit)
  if (nrow(miss_audit) > 0L && isTRUE(cfg$diagnostics$save_csvs))
    write_diag_csv(miss_audit, cfg, out_dir,
      cfg$diagnostics$missing_code_audit_csv %||% "targeted_missing_code_audit.csv")
  tail_audit <- build_earnings_tail_cap_audit(main_df, cfg, tmle_fit, A, w)
  if (nrow(tail_audit) > 0L && isTRUE(cfg$diagnostics$save_csvs))
    write_diag_csv(tail_audit, cfg, out_dir,
      cfg$diagnostics$earnings_tail_audit_csv %||% "earnings_tail_cap_audit.csv")
  cluster <- as.character(main_df[[cfg$analysis$cluster_var]])

  # ---- Sample flow summary -----------------------------------------------
  msg("  [diag] Building sample-flow summary...", cfg = cfg)
  flow <- attr(main_df, "sample_flow") %||% list()
  flow_names <- names(flow)
  sample_flow <- data.frame(
    step = flow_names,
    n = vapply(flow, function(z) as.numeric(z)[1L], numeric(1)),
    stringsAsFactors = FALSE)
  if (isTRUE(cfg$diagnostics$save_csvs))
    write_diag_csv(sample_flow, cfg, out_dir, "sample_flow.csv")
  if (!is.null(tmle_fit) && !is.null(tmle_fit$att_components)) {
    ac_audit <- tmle_fit$att_components
    capped <- is.finite(ac_audit$Y_raw) & ac_audit$Y_raw > ac_audit$y_upper
    mort_cfg <- cfg$mortality_sensitivity %||% list()
    interview_var <- mort_cfg$interview_year_var %||% "IYEAR4"
    interview_year <- if (interview_var %in% names(main_df))
      suppressWarnings(as.integer(main_df[[interview_var]])) else
      rep(NA_integer_, nrow(main_df))
    compact_year_counts <- function(x) {
      tab <- table(x[is.finite(x)], useNA = "no")
      if (!length(tab)) return(NA_character_)
      paste(paste0(names(tab), "=", as.integer(tab)), collapse = ";")
    }
    analysis_audit <- data.frame(
      wave1_merged = flow$wave1_merged %||% raw_w1_rows %||% NA_integer_,
      complete_wave2_cesd = flow$complete_wave2_cesd %||% NA_integer_,
      invalid_weight_dropped = flow$invalid_weight_dropped %||% NA_integer_,
      final_analytic_n = nrow(main_df),
      treated_n = sum(A == 1L), control_n = sum(A == 0L),
      outcome_observed_n = sum(delta_Y == 1L),
      outcome_missing_n = sum(delta_Y == 0L),
      earnings_exact_n = flow$earnings_exact_final %||% NA_integer_,
      earnings_bracket_n = flow$earnings_bracket_final %||% NA_integer_,
      mortality_zero_n = flow$mortality_zero_final %||% NA_integer_,
      valid_zero_exact_n = flow$valid_zero_exact_final %||% NA_integer_,
      cap_probability = tmle_fit$result$cap_probability[1L],
      cap_value = ac_audit$y_upper,
      capped_total_n = sum(capped),
      capped_treated_n = sum(capped & A == 1L),
      capped_control_n = sum(capped & A == 0L),
      instrument_ceiling_999995_total_n = sum(ac_audit$Y_raw == 999995, na.rm = TRUE),
      instrument_ceiling_999995_treated_n = sum(ac_audit$Y_raw == 999995 & A == 1L, na.rm = TRUE),
      instrument_ceiling_999995_control_n = sum(ac_audit$Y_raw == 999995 & A == 0L, na.rm = TRUE),
      analytic_psu_n = length(unique(cluster)),
      full_design_psu_n = flow$full_design_psu %||% NA_integer_,
      strata_n = length(unique(main_df[[cfg$analysis$strata_var]])),
      interview_year_variable = interview_var,
      interview_year_observed_n = sum(is.finite(interview_year)),
      interview_year_missing_n = sum(!is.finite(interview_year)),
      interview_year_counts = compact_year_counts(interview_year),
      interview_year_role = "audit_only_never_used_for_mortality_classification",
      mortality_classification_source = if (isTRUE(mort_cfg$enabled %||% FALSE))
        mort_cfg$source_var %||% "NDIDD19Y" else NA_character_,
      mortality_classification_window = if (isTRUE(mort_cfg$enabled %||% FALSE))
        sprintf("%d-%d inclusive", as.integer(mort_cfg$death_year_start),
                as.integer(mort_cfg$death_year_end)) else NA_character_,
      mortality_death_with_interview_year_n = if (isTRUE(mort_cfg$enabled %||% FALSE) &&
          mort_cfg$death_before_outcome_var %in% names(main_df))
        sum(main_df[[mort_cfg$death_before_outcome_var]] == 1L & is.finite(interview_year),
            na.rm = TRUE) else NA_integer_,
      mortality_death_without_interview_year_n = if (isTRUE(mort_cfg$enabled %||% FALSE) &&
          mort_cfg$death_before_outcome_var %in% names(main_df))
        sum(main_df[[mort_cfg$death_before_outcome_var]] == 1L & !is.finite(interview_year),
            na.rm = TRUE) else NA_integer_,
      dollar_price_year = NA_integer_,
      dollar_basis = mort_cfg$earnings_price_basis %||%
        "nominal_past_year_dollars_no_inflation_adjustment",
      inflation_adjustment_applied = FALSE,
      stringsAsFactors = FALSE)
    if (isTRUE(cfg$diagnostics$save_csvs))
      write_diag_csv(analysis_audit, cfg, out_dir,
        cfg$diagnostics$analysis_sample_audit_csv %||% "analysis_sample_audit.csv")
  }

  # ---- Effective sample size -----------------------------------------------
  ess_rows <- list(
    overall = data.frame(group = "overall", n = length(w), n_eff = kish_eff_n(w)),
    treated = data.frame(group = "A=1", n = sum(A == 1L), n_eff = kish_eff_n(w[A == 1L])),
    unexposed = data.frame(group = "A=0", n = sum(A == 0L), n_eff = kish_eff_n(w[A == 0L])),
    observed = data.frame(group = "delta_Y=1", n = sum(delta_Y == 1L), n_eff = kish_eff_n(w[delta_Y == 1L])),
    missing = data.frame(group = "delta_Y=0", n = sum(delta_Y == 0L), n_eff = kish_eff_n(w[delta_Y == 0L]))
  )
  if (!is.null(tmle_fit)) {
    for (vv in sort(unique(tmle_fit$outer_fold))) {
      ii <- tmle_fit$outer_fold == vv
      ess_rows[[paste0("fold_", vv)]] <- data.frame(group = paste0("outer_fold=", vv), n = sum(ii), n_eff = kish_eff_n(w[ii]))
    }
  }
  ess_df <- do.call(rbind, ess_rows)
  rownames(ess_df) <- NULL
  if (isTRUE(cfg$diagnostics$save_csvs)) {
    write_diag_csv(ess_df, cfg, out_dir, cfg$diagnostics$effective_sample_size_csv %||% "effective_sample_size.csv")
  }

  # ---- Selection consistency -----------------------------------------------
  if (!is.null(tmle_fit)) {
    msg("  [diag] Aggregating cross-fold variable-selection consistency...", cfg = cfg)
    if (!is.null(tmle_fit$selection_log) && nrow(tmle_fit$selection_log) > 0L) {
      sl <- tmle_fit$selection_log
      vars <- sort(unique(sl$variable))
      sel_cons <- data.frame(variable = vars, stringsAsFactors = FALSE)
      bool_cols <- intersect(c("selected", "in_rough_pool", "dropped_redundant", "selected_by_joint_AY",
                               "selected_by_outcome", "selected_by_delta", "selected_by_lasso_A",
                               "selected_by_lasso_Y", "selected_by_lasso_delta", "kept_in_final_W",
                               "dropped_by_column_cap"), names(sl))
      for (cc in bool_cols) {
        sel_cons[[paste0("n_folds_", cc)]] <- vapply(vars, function(vv) sum(isTRUE_vec(sl[sl$variable == vv, cc])), integer(1))
      }
      if ("selected" %in% bool_cols) sel_cons <- sel_cons[order(-sel_cons$n_folds_selected, sel_cons$variable), , drop = FALSE]
    } else {
      vars <- sort(unique(unlist(tmle_fit$selected_by_fold)))
      sel_cons <- data.frame(variable = vars,
                             n_folds_selected = vapply(vars, function(vv) sum(vapply(tmle_fit$selected_by_fold, function(z) vv %in% z, logical(1))), integer(1)),
                             stringsAsFactors = FALSE)
      sel_cons <- sel_cons[order(-sel_cons$n_folds_selected, sel_cons$variable), , drop = FALSE]
    }
    if (isTRUE(cfg$diagnostics$save_csvs)) {
      write_diag_csv(sel_cons, cfg, out_dir, cfg$diagnostics$variable_consistency_csv %||% "variable_selection_consistency.csv")
      if (!is.null(tmle_fit$selected_by_fold) && length(tmle_fit$selected_by_fold) > 1L) {
        pairs <- utils::combn(seq_along(tmle_fit$selected_by_fold), 2)
        jac <- do.call(rbind, lapply(seq_len(ncol(pairs)), function(j) {
          a <- unique(tmle_fit$selected_by_fold[[pairs[1, j]]]); b <- unique(tmle_fit$selected_by_fold[[pairs[2, j]]])
          u <- union(a, b); inter <- intersect(a, b)
          data.frame(fold_1 = pairs[1, j], fold_2 = pairs[2, j],
                     n_fold_1 = length(a), n_fold_2 = length(b),
                     n_intersection = length(inter), n_union = length(u),
                     jaccard = if (length(u) == 0L) NA_real_ else length(inter) / length(u),
                     stringsAsFactors = FALSE)
        }))
        write_diag_csv(jac, cfg, out_dir, cfg$diagnostics$selection_jaccard_csv %||% "selection_jaccard.csv")
      }
      # Within-seed fold-selection stability artifact. One row per run recording the
      # pipeline seed, the size of the selected confounder set (union across
      # folds), the core size (selected in ALL folds), the mean pairwise fold
      # Jaccard, and a fingerprint (count + first/last vars) of the sorted core.
      # This file is not an across-seed summary; the multiseed runner writes
      # separate variable-frequency and pairwise-Jaccard artifacts.
      nfold_sel <- length(tmle_fit$selected_by_fold %||% list())
      core_vars <- if (nfold_sel > 0L && "n_folds_selected" %in% names(sel_cons)) {
        sort(sel_cons$variable[sel_cons$n_folds_selected == nfold_sel])
      } else if (nfold_sel > 0L) {
        # Fallback: recompute core directly from selected_by_fold.
        allv <- sort(unique(unlist(tmle_fit$selected_by_fold)))
        sort(allv[vapply(allv, function(vv)
          all(vapply(tmle_fit$selected_by_fold, function(z) vv %in% z, logical(1))), logical(1))])
      } else character(0)
      union_vars <- sort(unique(unlist(tmle_fit$selected_by_fold %||% list())))
      mean_jac <- if (exists("jac") && is.data.frame(jac) && nrow(jac) > 0L)
        mean(jac$jaccard, na.rm = TRUE) else NA_real_
      seed_stab <- data.frame(
        pipeline_seed = cfg$global$pipeline_seed %||% NA_integer_,
        outcome_var = cfg$analysis$outcome_var,
        n_folds = nfold_sel,
        n_selected_union = length(union_vars),
        n_selected_core = length(core_vars),
        mean_pairwise_fold_jaccard = mean_jac,
        core_first_var = if (length(core_vars)) core_vars[1] else NA_character_,
        core_last_var = if (length(core_vars)) core_vars[length(core_vars)] else NA_character_,
        core_fingerprint_md5 = object_md5(core_vars),
        stringsAsFactors = FALSE)
      write_diag_csv(seed_stab, cfg, out_dir,
                     cfg$diagnostics$fold_selection_stability_csv %||% "fold_selection_stability.csv")
    }
  }

  # ---- Learner weights ------------------------------------------------------
  if (!is.null(tmle_fit) && !is.null(tmle_fit$sl_log)) {
    msg("  [diag] Summarizing SuperLearner meta-weights...", cfg = cfg)
    lw <- make_sl_weight_long(tmle_fit$sl_log)
    if (nrow(lw) > 0L) {
      nuis <- sort(unique(lw$nuisance))
      combos <- unique(lw[, c("nuisance", "learner"), drop = FALSE])
      lw_sum <- do.call(rbind, lapply(seq_len(nrow(combos)), function(ii) {
        nn <- combos$nuisance[ii]; ll <- combos$learner[ii]
        z <- lw$weight[lw$nuisance == nn & lw$learner == ll]
        data.frame(nuisance = nn, learner = ll, mean_weight = mean(z, na.rm = TRUE),
                   sd_weight = stats::sd(z, na.rm = TRUE), min_weight = min(z, na.rm = TRUE),
                   max_weight = max(z, na.rm = TRUE), n_folds_nonzero = sum(z > 1e-8, na.rm = TRUE),
                   stringsAsFactors = FALSE)
      }))
      if (isTRUE(cfg$diagnostics$save_csvs)) {
        write_diag_csv(lw, cfg, out_dir, cfg$diagnostics$learner_weight_long_csv %||% "learner_weight_long.csv")
        write_diag_csv(lw_sum, cfg, out_dir, cfg$diagnostics$learner_weight_summary_csv %||% "learner_weight_summary.csv")
      }
      if (isTRUE(cfg$diagnostics$save_plots)) {
        for (nn in nuis) {
          mat <- xtabs(weight ~ learner + fold, data = lw[lw$nuisance == nn, , drop = FALSE])
          plot_path <- build_unique_diag_path(cfg, out_dir, paste0("sl_weights_stacked_", nn, ".png"))
          png(plot_path, width = 1000, height = 650, res = 150)
          par(mar = c(4, 8, 3, 1))
          barplot(mat, legend.text = rownames(mat), args.legend = list(x = "topright", cex = 0.7),
                  xlab = "Outer fold", ylab = "SuperLearner weight", main = paste("SL weights:", nn), las = 2)
          dev.off()
        }
      }
    }
    lr <- make_sl_risk_long(tmle_fit$sl_log)
    if (nrow(lr) > 0L) {
      combos_r <- unique(lr[, c("nuisance", "learner"), drop = FALSE])
      lr_sum <- do.call(rbind, lapply(seq_len(nrow(combos_r)), function(ii) {
        nn <- combos_r$nuisance[ii]; ll <- combos_r$learner[ii]
        z <- lr$cv_risk[lr$nuisance == nn & lr$learner == ll]
        data.frame(nuisance = nn, learner = ll,
                   mean_cv_risk = mean(z, na.rm = TRUE),
                   median_cv_risk = stats::median(z, na.rm = TRUE),
                   min_cv_risk = min(z, na.rm = TRUE),
                   max_cv_risk = max(z, na.rm = TRUE),
                   n_folds = sum(is.finite(z)), stringsAsFactors = FALSE)
      }))
      if (isTRUE(cfg$diagnostics$save_csvs)) {
        write_diag_csv(lr, cfg, out_dir,
          cfg$diagnostics$learner_risk_long_csv %||% "learner_cv_risk_long.csv")
        write_diag_csv(lr_sum, cfg, out_dir,
          cfg$diagnostics$learner_risk_summary_csv %||% "learner_cv_risk_summary.csv")
      }
    }
  }

  if (!is.null(tmle_fit) && !is.null(tmle_fit$sl_log) && isTRUE(cfg$diagnostics$save_csvs)) {
    outer_validation <- make_outer_validation_summary(tmle_fit$sl_log, tmle_fit)
    if (nrow(outer_validation))
      write_diag_csv(outer_validation, cfg, out_dir,
        cfg$diagnostics$nuisance_outer_validation_csv %||% "nuisance_outer_validation_summary.csv")
  }

  # ---- Per-fold plug-in ATE diagnostics ------------------------------------
  if (!is.null(tmle_fit)) {
    folds <- sort(unique(tmle_fit$outer_fold))
    fold_ate <- do.call(rbind, lapply(folds, function(vv) {
      ii <- tmle_fit$outer_fold == vv
      ww <- w[ii]
      psi <- sum(ww * (tmle_fit$Qstar1W_orig[ii] - tmle_fit$Qstar0W_orig[ii]), na.rm = TRUE) / sum(ww, na.rm = TRUE)
      data.frame(fold = vv, n = sum(ii), n_treated = sum(A[ii] == 1L), n_eff = kish_eff_n(ww), plugin_ate = psi)
    }))
    if (isTRUE(cfg$diagnostics$save_csvs)) {
      write_diag_csv(fold_ate, cfg, out_dir, cfg$diagnostics$per_fold_ate_csv %||% "per_fold_ate_diagnostics.csv")
    }
  }

  # ---- ATT-specific diagnostics --------------------------------------------
  # All computed from the canonical per-row ATT components saved by the TMLE
  # routine (att_components), so every number is consistent with the headline
  # ATT and its EIF SE. ac$D_att is the efficient influence function; the
  # numerator of psi_att is (A*att_summand + att_resid); psi_att normalizes by
  # sum(w_norm*A). Restricting any of these to a subset and renormalizing gives
  # the subset's one-step ATT.
  if (!is.null(tmle_fit) && !is.null(tmle_fit$att_components)) {
    ac  <- tmle_fit$att_components
    Aa  <- ac$A; wn <- ac$w_norm; of <- ac$outer_fold; cl <- ac$cluster
    num <- Aa * ac$att_summand + ac$att_resid    # per-row numerator of psi_att
    den_all <- sum(wn * Aa)                        # denominator = weighted treated count

    # Helper: one-step ATT on an arbitrary row-subset (logical or index vector).
    att_on_subset <- function(keep) {
      d <- sum(wn[keep] * Aa[keep])
      if (!is.finite(d) || d <= 0) return(NA_real_)
      sum(wn[keep] * num[keep]) / d
    }

    # (1) Per-fold ATT diagnostics ------------------------------------------
    folds_a <- sort(unique(of))
    # for a binary outcome, log per-fold treated/control event counts
    # among OBSERVED rows (delta_Y == 1), so sparse event cells are visible
    # before a fold is trusted. Columns are NA for non-binary outcomes.
    yraw_a <- ac$Y_raw; dobs_a <- ac$delta_Y
    is_bin_out <- any(dobs_a == 1L, na.rm = TRUE) &&
      all(yraw_a[is.finite(yraw_a) & dobs_a == 1L] %in% c(0, 1))
    att_per_fold <- do.call(rbind, lapply(folds_a, function(vv) {
      ii <- of == vv
      data.frame(
        fold = vv,
        n = sum(ii),
        n_treated = sum(Aa[ii] == 1L),
        n_control = sum(Aa[ii] == 0L),
        ess_treated = kish_eff_n(ac$weights[ii][Aa[ii] == 1L]),
        att_fold = att_on_subset(which(ii)),
        n_treated_obs = sum(ii & Aa == 1L & dobs_a == 1L, na.rm = TRUE),
        n_control_obs = sum(ii & Aa == 0L & dobs_a == 1L, na.rm = TRUE),
        n_treated_event = if (is_bin_out) sum(ii & Aa == 1L & dobs_a == 1L & yraw_a == 1, na.rm = TRUE) else NA_integer_,
        n_treated_nonevent = if (is_bin_out) sum(ii & Aa == 1L & dobs_a == 1L & yraw_a == 0, na.rm = TRUE) else NA_integer_,
        n_control_event = if (is_bin_out) sum(ii & Aa == 0L & dobs_a == 1L & yraw_a == 1, na.rm = TRUE) else NA_integer_,
        n_control_nonevent = if (is_bin_out) sum(ii & Aa == 0L & dobs_a == 1L & yraw_a == 0, na.rm = TRUE) else NA_integer_,
        stringsAsFactors = FALSE)
    }))
    if (isTRUE(cfg$diagnostics$save_csvs))
      write_diag_csv(att_per_fold, cfg, out_dir,
        cfg$diagnostics$att_per_fold_csv %||% "att_per_fold_diagnostics.csv")

    # (2) Per-cluster ATT influence (all clusters) --------------------------
    # cl_eic_att[k] = sum over rows in cluster k of (w_norm * D_att); the ATT
    # SE is built from these. Report each cluster's contribution and its
    # leave-one-cluster-out delta (linear approximation via the EIF sum).
    cl_names <- names(ac$cl_eic_att)
    cl_contrib <- as.numeric(ac$cl_eic_att)
    # Exact leave-one-out ATT for each cluster (drop the cluster, renormalize).
    loo_one <- vapply(cl_names, function(cn) att_on_subset(cl != cn), numeric(1))
    att_cl_all <- data.frame(
      cluster = cl_names,
      n_rows = as.integer(tapply(rep(1L, length(cl)), cl, sum)[cl_names]),
      n_treated = as.integer(tapply(Aa, cl, sum)[cl_names]),
      eic_contribution = cl_contrib,
      abs_eic_contribution = abs(cl_contrib),
      att_leave_this_cluster_out = loo_one,
      delta_from_full = loo_one - ac$psi_att,
      stringsAsFactors = FALSE)
    att_cl_all <- att_cl_all[order(-att_cl_all$abs_eic_contribution), , drop = FALSE]
    if (isTRUE(cfg$diagnostics$save_csvs))
      write_diag_csv(att_cl_all, cfg, out_dir,
        cfg$diagnostics$att_cluster_influence_all_csv %||% "att_cluster_influence_all.csv")

    # (2b) Inference robustness diagnostics (finite-sample, no nuisance refit) ----
    # The primary interval is the REGION-stratified, PSUSCID-clustered,
    # with-replacement survey-design EIF interval saved by the estimator.
    # G* is reported only as an influence-concentration diagnostic. The LOCO
    # and multiplier calculations hold all fitted nuisances fixed and therefore
    # are supplemental stability diagnostics rather than full-pipeline resampling.
    Sg <- cl_contrib
    n_rows_att <- length(cl)
    G_cl <- length(Sg)
    sumS2 <- sum(Sg^2); sumS4 <- sum(Sg^4)
    G_star <- if (is.finite(sumS4) && sumS4 > 0) (sumS2^2) / sumS4 else NA_real_
    se_analytic <- ac$att_se
    lo <- loo_one[is.finite(loo_one)]
    Gj <- length(lo)
    se_loco <- if (Gj > 1L) sqrt((Gj - 1) / Gj * sum((lo - mean(lo))^2)) else NA_real_
    Bboot <- 2000L
    boot <- with_local_seed(seed_for(cfg, 424242L),
      vapply(seq_len(Bboot), function(b) {
        xi <- sample(c(-1, 1), G_cl, replace = TRUE)
        ac$psi_att + sum(xi * Sg) / n_rows_att
      }, numeric(1)))
    se_boot <- stats::sd(boot)
    boot_ci <- stats::quantile(boot, c(0.025, 0.975), names = FALSE, type = 7)
    s2sorted <- sort(Sg^2, decreasing = TRUE)
    share_k <- function(k) if (is.finite(sumS2) && sumS2 > 0)
      sum(s2sorted[seq_len(min(k, G_cl))]) / sumS2 else NA_real_
    gstar_df <- if (is.finite(G_star) && G_star > 1) G_star - 1 else NA_real_
    gstar_tcrit <- if (is.finite(gstar_df)) stats::qt(0.975, df = gstar_df) else NA_real_
    gstar_ci <- if (is.finite(gstar_tcrit)) ac$psi_att + c(-1, 1) * gstar_tcrit * se_analytic else c(NA_real_, NA_real_)
    gstar_p <- if (is.finite(gstar_df) && is.finite(se_analytic) && se_analytic > 0)
      2 * stats::pt(-abs(ac$psi_att / se_analytic), df = gstar_df) else NA_real_
    att_inf <- data.frame(
      psi_att = ac$psi_att,
      n_rows = n_rows_att,
      G_nominal = G_cl,
      G_star_influence_concentration = G_star,
      top1_variance_share = share_k(1L),
      top5_variance_share = share_k(5L),
      top10_variance_share = share_k(10L),
      primary_inference_method = ac$inference_method,
      primary_df = ac$inference_df,
      primary_se = se_analytic,
      primary_ci_lower = ac$inference_ci[1L],
      primary_ci_upper = ac$inference_ci[2L],
      primary_p = ac$inference_p,
      gstar_df_sensitivity = gstar_df,
      gstar_t_critical_sensitivity = gstar_tcrit,
      gstar_ci_lower_sensitivity = gstar_ci[1L],
      gstar_ci_upper_sensitivity = gstar_ci[2L],
      gstar_p_sensitivity = gstar_p,
      psu_only_se_sensitivity = ac$se_psu_only_sensitivity,
      psu_only_ci_lower_sensitivity = ac$ci_psu_only_sensitivity[1L],
      psu_only_ci_upper_sensitivity = ac$ci_psu_only_sensitivity[2L],
      psu_only_p_sensitivity = ac$p_psu_only_sensitivity,
      fixed_nuisance_loco_se = se_loco,
      fixed_nuisance_multiplier_se = se_boot,
      fixed_nuisance_multiplier_ci_lower = boot_ci[1L],
      fixed_nuisance_multiplier_ci_upper = boot_ci[2L],
      multiplier_reps = Bboot,
      stringsAsFactors = FALSE)
    if (isTRUE(cfg$diagnostics$save_csvs))
      write_diag_csv(att_inf, cfg, out_dir, "att_inference_robustness.csv")

    # (3) Top influential clusters ------------------------------------------
    top_k_show <- min(10L, nrow(att_cl_all))
    if (isTRUE(cfg$diagnostics$save_csvs))
      write_diag_csv(utils::head(att_cl_all, top_k_show), cfg, out_dir,
        cfg$diagnostics$att_top_cluster_influence_csv %||% "top_att_cluster_influence.csv")

    # (4) Leave-top-k-clusters-out ATT --------------------------------------
    ranked_clusters <- att_cl_all$cluster   # already sorted by |contribution|
    kvec <- cfg$diagnostics$att_leaveout_top_k %||% c(1L, 3L, 5L)
    att_leaveout <- do.call(rbind, lapply(kvec, function(kk) {
      kk <- min(kk, length(ranked_clusters))
      drop_cl <- ranked_clusters[seq_len(kk)]
      keep <- !(cl %in% drop_cl)
      est <- att_on_subset(which(keep))
      data.frame(
        drop_top_abs_clusters = kk,
        dropped_clusters = paste(drop_cl, collapse = ";"),
        n_remaining = sum(keep),
        att_remaining = est,
        difference_from_full = est - ac$psi_att,
        stringsAsFactors = FALSE)
    }))
    if (isTRUE(cfg$diagnostics$save_csvs))
      write_diag_csv(att_leaveout, cfg, out_dir,
        cfg$diagnostics$att_cluster_influence_leaveout_csv %||% "att_cluster_influence_leaveout.csv")

    # (5) Estimator decomposition -------------------------------------------
    # psi_att = [ contrast_term + treated_resid_term - control_resid_term ] / den
    # where the three numerator pieces are the weighted sums. The control resid
    # term enters att_resid with a minus sign; report its signed contribution.
    contrast_term  <- sum(wn * Aa * ac$att_summand)
    treated_resid  <- sum(wn * (Aa * ac$resid_obs / ac$pi_AW))
    control_resid  <- sum(wn * ((1 - Aa) * (ac$gn / (1 - ac$gn)) * ac$resid_obs / ac$pi_AW))
    att_decomp <- data.frame(
      component = c("contrast_term_sum", "treated_residual_term_sum",
                    "control_residual_term_sum", "denominator_weighted_treated",
                    "contrast_term_per_treated", "treated_residual_per_treated",
                    "control_residual_per_treated", "psi_att_total"),
      value = c(contrast_term, treated_resid, control_resid, den_all,
                contrast_term / den_all, treated_resid / den_all,
                control_resid / den_all,
                (contrast_term + treated_resid - control_resid) / den_all),
      stringsAsFactors = FALSE)
    if (isTRUE(cfg$diagnostics$save_csvs))
      write_diag_csv(att_decomp, cfg, out_dir,
        cfg$diagnostics$att_estimator_decomposition_csv %||% "att_estimator_decomposition.csv")

    # (6) EIF diagnostics ----------------------------------------------------
    Dw <- wn * ac$D_att
    eif_sd_w <- sqrt(sum((Dw - mean(Dw))^2) / length(Dw))
    att_eif_diag <- data.frame(
      metric = c("psi_att", "att_se", "att_se_centered", "eif_weighted_mean",
                 "pi_set_to_one_wiring_check", "pi_set_to_one_wiring_gap",
                 "n_clusters", "small_sample_correction",
                 "eif_min", "eif_max", "eif_sd_unweighted",
                 "weighted_eif_sd", "max_abs_cluster_eic",
                 "sum_cluster_eic_sq"),
      value = c(ac$psi_att, ac$att_se, ac$att_se_centered %||% NA_real_, ac$eif_mean, ac$nocens,
                ac$nocens - ac$psi_att, ac$J_att, ac$fsc_att,
                min(ac$D_att), max(ac$D_att), stats::sd(ac$D_att),
                eif_sd_w, max(abs(ac$cl_eic_att)),
                sum(ac$cl_eic_att^2)),
      stringsAsFactors = FALSE)
    if (isTRUE(cfg$diagnostics$save_csvs))
      write_diag_csv(att_eif_diag, cfg, out_dir,
        cfg$diagnostics$att_eif_diagnostics_csv %||% "att_eif_diagnostics.csv")

    # (7) ATT control-weight ESS by fold ------------------------------------
    # Under the ATT, controls carry odds-weights g/(1-g) (times survey weight).
    # The Kish ESS of those weights among controls in each fold measures how
    # much independent information the reweighted control pool actually carries.
    att_ctrl_ess <- do.call(rbind, lapply(folds_a, function(vv) {
      ii <- of == vv & Aa == 0L
      ow <- (ac$gn[ii] / (1 - ac$gn[ii])) * ac$weights[ii]
      data.frame(
        fold = vv,
        n_control = sum(ii),
        control_oddsw_sum = sum(ow),
        control_oddsw_max = if (any(ii)) max(ow) else NA_real_,
        ess_control_attweighted = kish_eff_n(ow),
        stringsAsFactors = FALSE)
    }))
    if (isTRUE(cfg$diagnostics$save_csvs))
      write_diag_csv(att_ctrl_ess, cfg, out_dir,
        cfg$diagnostics$att_weighted_control_ess_by_fold_csv %||% "att_weighted_control_ess_by_fold.csv")

    # (8) Censoring-model positivity and calibration ---------------------
    # Report both the IPCW distribution and actual held-out calibration of
    # pi(A,W) against delta_Y. Decile rows use equal-count bins but weighted
    # predicted and observed means, while summary rows include weighted Brier
    # and log loss.
    pi_arm_diagnostics <- function(mask, label) {
      pim <- as.numeric(ac$pi_AW[mask])
      yy <- as.integer(ac$delta_Y[mask])
      ww <- normalize_positive_weights(ac$weights[mask], sum(mask), paste0("pi diagnostic ", label))
      if (!length(pim) || any(!is.finite(pim)) || any(!yy %in% c(0L, 1L)))
        stop("Censoring calibration received invalid held-out predictions or outcomes.", call. = FALSE)
      eps <- 1e-12
      pc <- pmin(pmax(pim, eps), 1 - eps)
      ipw <- 1 / pc
      wmean <- function(z) sum(ww * z) / sum(ww)
      summary_row <- data.frame(
        row_type = "summary", arm = label, bin = NA_integer_, n = length(pim),
        weight_sum = sum(ww), predicted_mean = wmean(pim),
        observed_rate = wmean(yy), brier = wmean((yy - pim)^2),
        log_loss = -wmean(yy * log(pc) + (1 - yy) * log(1 - pc)),
        pi_min = min(pim), pi_p01 = as.numeric(stats::quantile(pim, 0.01, names = FALSE, type = 8)),
        pi_median = stats::median(pim), pi_max = max(pim),
        ipcw_median = stats::median(ipw),
        ipcw_p99 = as.numeric(stats::quantile(ipw, 0.99, names = FALSE, type = 8)),
        ipcw_max = max(ipw), ipcw_frac_gt_10 = wmean(ipw > 10),
        ipcw_frac_gt_20 = wmean(ipw > 20), stringsAsFactors = FALSE)
      nb <- min(10L, length(pim))
      bins <- ceiling(rank(pim, ties.method = "first") * nb / length(pim))
      bin_rows <- do.call(rbind, lapply(seq_len(nb), function(bb) {
        ii <- bins == bb; wwb <- ww[ii]
        data.frame(
          row_type = "calibration_bin", arm = label, bin = bb, n = sum(ii),
          weight_sum = sum(wwb),
          predicted_mean = sum(wwb * pim[ii]) / sum(wwb),
          observed_rate = sum(wwb * yy[ii]) / sum(wwb),
          brier = NA_real_, log_loss = NA_real_,
          pi_min = min(pim[ii]), pi_p01 = NA_real_, pi_median = stats::median(pim[ii]),
          pi_max = max(pim[ii]), ipcw_median = NA_real_, ipcw_p99 = NA_real_,
          ipcw_max = NA_real_, ipcw_frac_gt_10 = NA_real_, ipcw_frac_gt_20 = NA_real_,
          stringsAsFactors = FALSE)
      }))
      rbind(summary_row, bin_rows)
    }
    att_pi_cal <- rbind(
      pi_arm_diagnostics(Aa == 1L, "treated"),
      pi_arm_diagnostics(Aa == 0L, "control"),
      pi_arm_diagnostics(rep(TRUE, length(Aa)), "all"))
    if (isTRUE(cfg$diagnostics$save_csvs))
      write_diag_csv(att_pi_cal, cfg, out_dir,
        cfg$diagnostics$att_pi_positivity_calibration_csv %||% "att_pi_positivity_calibration.csv")
    pi_contrast <- run_optional("pi_treatment_contrast", function() {
      assert_required_columns(
        ac, c("outer_fold", "weights", "pi_1W", "pi_0W"),
        "ATT components for pi treatment contrast")
      if (is.null(tmle_fit$pi_1W_raw) || is.null(tmle_fit$pi_0W_raw))
        stop("Pi treatment contrast requires raw counterfactual censoring predictions.",
             call. = FALSE)
      n_pi <- length(ac$outer_fold)
      pi_vectors <- list(
        weights = ac$weights, pi_1W = ac$pi_1W, pi_0W = ac$pi_0W,
        pi_1W_raw = tmle_fit$pi_1W_raw, pi_0W_raw = tmle_fit$pi_0W_raw)
      bad_lengths <- names(pi_vectors)[vapply(
        pi_vectors, length, integer(1)) != n_pi]
      bad_values <- names(pi_vectors)[vapply(
        pi_vectors, function(z) any(!is.finite(as.numeric(z))), logical(1))]
      if (length(bad_lengths) || length(bad_values))
        stop(paste0(
          "Pi treatment contrast received misaligned or nonfinite vectors. ",
          "Length failures: ", paste(bad_lengths, collapse = ", "),
          "; nonfinite failures: ", paste(bad_values, collapse = ", "), "."),
          call. = FALSE)
      folds_pi <- sort(unique(as.integer(ac$outer_fold)))
      if (!length(folds_pi) || any(!is.finite(folds_pi)))
        stop("Pi treatment contrast found no valid outer-fold identifiers.",
             call. = FALSE)
      pi_contrast_rows <- do.call(rbind, lapply(folds_pi, function(vv) {
        ii <- ac$outer_fold == vv
        ww <- normalize_positive_weights(
          ac$weights[ii], sum(ii), paste0("pi contrast fold ", vv))
        d_raw <- tmle_fit$pi_1W_raw[ii] - tmle_fit$pi_0W_raw[ii]
        d_clip <- ac$pi_1W[ii] - ac$pi_0W[ii]
        data.frame(
          scope = "fold", fold = vv, n = sum(ii),
          weighted_mean_raw = sum(ww * d_raw) / sum(ww),
          mean_abs_raw = mean(abs(d_raw)), max_abs_raw = max(abs(d_raw)),
          fraction_abs_raw_gt_tol = mean(
            abs(d_raw) > (cfg$diagnostics$pi_treatment_invariance_tolerance %||% 1e-10)),
          weighted_mean_clipped = sum(ww * d_clip) / sum(ww),
          mean_abs_clipped = mean(abs(d_clip)),
          max_abs_clipped = max(abs(d_clip)),
          stringsAsFactors = FALSE)
      }))
      if (!is.data.frame(pi_contrast_rows) || nrow(pi_contrast_rows) != length(folds_pi))
        stop("Pi treatment contrast failed to produce one row per outer fold.",
             call. = FALSE)
      ww_all <- normalize_positive_weights(
        ac$weights, length(ac$weights), "pi contrast overall")
      d_raw_all <- tmle_fit$pi_1W_raw - tmle_fit$pi_0W_raw
      d_clip_all <- ac$pi_1W - ac$pi_0W
      out <- rbind(
        pi_contrast_rows,
        data.frame(
          scope = "overall", fold = NA_integer_, n = length(d_raw_all),
          weighted_mean_raw = sum(ww_all * d_raw_all) / sum(ww_all),
          mean_abs_raw = mean(abs(d_raw_all)),
          max_abs_raw = max(abs(d_raw_all)),
          fraction_abs_raw_gt_tol = mean(
            abs(d_raw_all) > (cfg$diagnostics$pi_treatment_invariance_tolerance %||% 1e-10)),
          weighted_mean_clipped = sum(ww_all * d_clip_all) / sum(ww_all),
          mean_abs_clipped = mean(abs(d_clip_all)),
          max_abs_clipped = max(abs(d_clip_all)),
          stringsAsFactors = FALSE))
      if (max(abs(d_raw_all)) <
          (cfg$diagnostics$pi_treatment_invariance_tolerance %||% 1e-10))
        message(paste0(
          "WARNING: The fitted censoring model is invariant to A to numerical ",
          "tolerance; review pi_treatment_contrast.csv."))
      if (isTRUE(cfg$diagnostics$save_csvs))
        write_diag_csv(
          out, cfg, out_dir,
          cfg$diagnostics$pi_treatment_contrast_csv %||%
            "pi_treatment_contrast.csv")
      out
    })

    # (9) Joint g/pi clipping sensitivity ----------------------------------
    # Reuses the same raw held-out nuisance predictions and Q fits, then
    # re-clips, re-targets both ATT components, and recomputes the EIF. This
    # isolates sensitivity to the positivity bound without rerunning screening
    # or SuperLearner and is therefore fast and directly interpretable.
    # Fail loudly if the sweep cannot re-target or if the configured row does
    # not reproduce the headline ATT. A silent/empty positivity sensitivity
    # would be misleading in a production diagnostics package.
    clip_sens <- build_att_g_pi_clip_sensitivity(tmle_fit, cfg)
    if (nrow(clip_sens) > 0L && isTRUE(cfg$diagnostics$save_csvs))
      write_diag_csv(clip_sens, cfg, out_dir,
        cfg$diagnostics$att_g_pi_clip_sensitivity_csv %||% "att_g_pi_clip_sensitivity.csv")

    # (10) Tail-perturbation diagnostic (NO nuisance refit) ------------------
    # The headline uses one canonical outcome capped at the configured primary
    # quantile in Q fitting, targeting, and the EIF. This inexpensive diagnostic
    # perturbs only the residual outcome cap while holding the fitted nuisance
    # functions and targeted contrast fixed. It is therefore explicitly NOT a
    # full alternative-estimand analysis. The curated sensitivity runner below
    # performs full refits at q0.990 and q1.000.
    if (identical(tmle_fit$outcome_type, "continuous")) {
      qs <- cfg$diagnostics$att_outcome_bound_quantiles %||% c(0.95, 0.975, 0.98, 0.99, 0.995)
      primary_q <- as.numeric(cfg$outcome$continuous_upper_quantile %||% NA_real_)
      if (is.finite(primary_q)) qs <- qs[abs(qs - primary_q) > 1e-12]
      obs_mask <- ac$delta_Y == 1L & is.finite(ac$Y_raw)
      y_obs_vals <- ac$Y_raw[obs_mask]
      y_obs_weights <- ac$weights[obs_mask]
      att_bound_q <- do.call(rbind, lapply(qs, function(q) {
        cap <- compute_continuous_cap(y_obs_vals, y_obs_weights, q, cfg)
        y_w <- ac$Y_bounded_orig
        y_w[obs_mask] <- pmin(ac$Y_raw[obs_mask], cap)
        resid_w  <- ifelse(obs_mask, y_w - ac$QbarAW_orig, 0)
        aresid_w <- (Aa * resid_w / ac$pi_AW) -
                    ((1 - Aa) * (ac$gn / (1 - ac$gn)) * resid_w / ac$pi_AW)
        psi_w <- sum(wn * (Aa * ac$att_summand + aresid_w)) / den_all
        data.frame(
          cap_probability = q,
          cap_weighted = isTRUE(cfg$outcome$continuous_cap_weighted %||% TRUE),
          cap_quantile_rule = cfg$outcome$continuous_cap_qrule %||% "hf8",
          outcome_cap = cap,
          att_estimate = psi_w,
          difference_from_headline = psi_w - ac$psi_att,
          stringsAsFactors = FALSE)
      }))
      att_bound_ref <- data.frame(
        cap_probability = cfg$outcome$continuous_upper_quantile,
        cap_weighted = isTRUE(cfg$outcome$continuous_cap_weighted %||% TRUE),
        cap_quantile_rule = cfg$outcome$continuous_cap_qrule %||% "hf8",
        outcome_cap = tmle_fit$result$cap_value[1L],
        att_estimate = ac$psi_att,
        difference_from_headline = 0,
        stringsAsFactors = FALSE)
      att_bound <- rbind(att_bound_ref, att_bound_q)
      if (isTRUE(cfg$diagnostics$save_csvs))
        write_diag_csv(att_bound, cfg, out_dir,
          cfg$diagnostics$att_outcome_bound_sensitivity_csv %||% "att_tail_perturbation_diagnostic.csv")
    } else {
      msg("  [diag] Outcome-bound sensitivity skipped (binary outcome).", cfg = cfg)
    }

    msg("  [diag] ATT-specific diagnostics written (per-fold, cluster influence, leave-out, decomposition, EIF, control ESS, pi calibration, g/pi clip sweep, tail perturbation).", cfg = cfg)
  }

  # ---- Run manifest --------------------------------------------------------
  # One per-outcome, per-fold design-verification table: did each fold respect
  # the cap, keep adequate support, and did the run pass its global ATT checks.
  if (!is.null(tmle_fit) && !is.null(tmle_fit$run_manifest)) {
    man <- tmle_fit$run_manifest
    ac2 <- tmle_fit$att_components
    man$att_estimate        <- if (!is.null(ac2)) ac2$psi_att else NA_real_
    man$att_se              <- if (!is.null(ac2)) ac2$att_se else NA_real_
    man$att_mu1 <- if (!is.null(ac2)) ac2$mu1_att else NA_real_
    man$att_mu0 <- if (!is.null(ac2)) ac2$mu0_att else NA_real_
    man$mu1_earnings_depressed <- if (!is.null(ac2) && identical(cfg$outcome$family, "Compensation")) ac2$mu1_att else NA_real_
    man$mu0_earnings_no_depression <- if (!is.null(ac2) && identical(cfg$outcome$family, "Compensation")) ac2$mu0_att else NA_real_
    man$pct_depression_effect <- if (!is.null(ac2)) ac2$pct_depression_effect else NA_real_
    man$pct_prevention_gain <- if (!is.null(ac2)) ac2$pct_prevention_gain else NA_real_
    man$att_target_score_mu1 <- if (!is.null(ac2)) ac2$target_score_mu1 else NA_real_
    man$att_target_score_mu0 <- if (!is.null(ac2)) ac2$target_score_mu0 else NA_real_
    man$att_eif_weighted_mean <- if (!is.null(ac2)) ac2$eif_mean else NA_real_
    man$att_eif_weighted_mean_scaled <- if (!is.null(ac2)) ac2$eif_mean_scaled else NA_real_
    man$att_centering_tolerance_scaled <- if (!is.null(ac2)) ac2$center_tol_scaled else NA_real_
    man$att_centering_pass <- if (!is.null(ac2))
      is.finite(ac2$eif_mean_scaled) && abs(ac2$eif_mean_scaled) <= ac2$center_tol_scaled else NA
    man$att_pi_set_to_one_wiring_gap <- if (!is.null(ac2)) ac2$nocens - ac2$psi_att else NA_real_
    man$outcome_var <- cfg$analysis$outcome_var
    if (isTRUE(cfg$diagnostics$save_csvs))
      write_diag_csv(man, cfg, out_dir, cfg$diagnostics$run_manifest_csv %||% "run_manifest.csv")
    msg("  [diag] Run manifest written (per-fold cap/support + global ATT checks).", cfg = cfg)
  }

  # ---- Balance diagnostics and Love plots ----------------------------------
  if (!is.null(tmle_fit)) {
    msg("  [diag] Computing numeric and categorical balance diagnostics...", cfg = cfg)
    love_plot_excluded_vars <- derive_love_plot_exclusions(tmle_fit, cfg)
    if (length(love_plot_excluded_vars))
      msg(sprintf("  [diag] Excluding %d variables rejected as too-missing in every fold from love-plot figures; full CSV audits retain them.",
                  length(love_plot_excluded_vars)), cfg = cfg)
    iptw <- ifelse(A == 1L, 1 / tmle_fit$gn, 1 / (1 - tmle_fit$gn))
    treat_post_w <- w * iptw
    balance_priority <- unique(c(
      cfg$final_tmle$protected_W %||% character(0),
      unlist(tmle_fit$selected_by_fold %||% list(), use.names = FALSE)))
    bal_A <- make_balance_table(main_df, cfg, A, treat_post_w, "A=1_vs_A=0",
                                priority_vars = balance_priority)
    record_balance_scan(bal_A, "A=1_vs_A=0", nrow(main_df))
    bal_A <- annotate_balance_for_love_plot(bal_A, love_plot_excluded_vars)
    if (isTRUE(cfg$diagnostics$save_csvs)) {
      write_diag_csv(bal_A, cfg, out_dir, cfg$diagnostics$balance_treatment_csv %||% "balance_treatment_loveplot_data.csv")
      lev_A <- attr(bal_A, "factor_level_balance")
      if (!is.null(lev_A) && nrow(lev_A))
        write_diag_csv(lev_A, cfg, out_dir,
          cfg$diagnostics$balance_factor_levels_treatment_csv %||% "balance_factor_levels_treatment.csv")
    }
    if (isTRUE(cfg$diagnostics$save_plots) && nrow(bal_A) > 0L) {
      plot_love(bal_A, build_unique_diag_path(cfg, out_dir, "love_plot_treatment.png"), "Treatment balance: pre vs post propensity weighting", n_top = cfg$diagnostics$max_love_plot_variables %||% 30L)
    }
    # treatment balance recomputed on the OVERLAP-TRIMMED sample (rows
    # with g in the trim band). This is the balance for the trimmed/ATT
    # headline estimand; extreme-weight rows from the no-overlap region are
    # removed, so rare-indicator imbalance (CUBAN/DISABLE-type) should be far
    # better behaved than on the full sample. This is the plot to report.
    if (isTRUE(cfg$diagnostics$balance_on_trimmed %||% TRUE) &&
        isTRUE(cfg$final_tmle$trim_enable %||% FALSE)) {
      tlo <- cfg$final_tmle$trim_g_lower %||% 0.05
      thi <- cfg$final_tmle$trim_g_upper %||% 0.95
      keep_bal <- tmle_fit$gn >= tlo & tmle_fit$gn <= thi
      n_keep_bal <- sum(keep_bal)
      if (n_keep_bal >= 2L && length(unique(A[keep_bal])) == 2L) {
        bal_A_trim <- make_balance_table(
          main_df[keep_bal, , drop = FALSE], cfg,
          A[keep_bal], treat_post_w[keep_bal], "A=1_vs_A=0_trimmed",
          priority_vars = balance_priority)
        record_balance_scan(bal_A_trim, "A=1_vs_A=0_trimmed", n_keep_bal)
        bal_A_trim <- annotate_balance_for_love_plot(
          bal_A_trim, love_plot_excluded_vars)
        msg(sprintf("  [diag] Trimmed-sample treatment balance on %d of %d rows (g in [%.2f, %.2f]).",
                    n_keep_bal, length(A), tlo, thi), cfg = cfg)
        if (isTRUE(cfg$diagnostics$save_csvs)) {
          write_diag_csv(bal_A_trim, cfg, out_dir,
            cfg$diagnostics$balance_treatment_trimmed_csv %||% "balance_treatment_trimmed_loveplot_data.csv")
          lev_trim <- attr(bal_A_trim, "factor_level_balance")
          if (!is.null(lev_trim) && nrow(lev_trim))
            write_diag_csv(lev_trim, cfg, out_dir,
              cfg$diagnostics$balance_factor_levels_trimmed_csv %||% "balance_factor_levels_treatment_trimmed.csv")
        }
        if (isTRUE(cfg$diagnostics$save_plots) && nrow(bal_A_trim) > 0L) {
          plot_love(bal_A_trim,
            build_unique_diag_path(cfg, out_dir, "love_plot_treatment_trimmed.png"),
            "Treatment balance (overlap-trimmed): pre vs post weighting",
            n_top = cfg$diagnostics$max_love_plot_variables %||% 30L)
        }
      } else {
        msg("  [diag] Trimmed-sample balance skipped: too few rows or no treatment variation in band.", cfg = cfg)
      }
    }
    # ATT-WEIGHTED balance -- the balance for the ATT headline estimand.
    # Under the ATT, treated units keep weight 1 and controls are reweighted by
    # the odds g/(1-g) to resemble the treated covariate distribution. This is
    # the correct balance diagnostic when the ATT is primary (distinct from the
    # full-sample ATE balance and the trimmed balance above). Survey weights are
    # carried through. Controls with g near 1 receive large odds weights; the
    # control-weight extremes are reported separately below as a positivity-style
    # diagnostic for the ATT.
    if (isTRUE(cfg$diagnostics$balance_att %||% TRUE)) {
      att_bal_w <- w * ifelse(A == 1L, 1, tmle_fit$gn / pmax(1 - tmle_fit$gn, 1e-6))
      if (length(unique(A)) == 2L) {
        bal_A_att <- make_balance_table(main_df, cfg, A, att_bal_w, "A=1_vs_A=0_ATT",
                                        priority_vars = balance_priority)
        record_balance_scan(bal_A_att, "A=1_vs_A=0_ATT", nrow(main_df))
        bal_A_att <- annotate_balance_for_love_plot(
          bal_A_att, love_plot_excluded_vars)
        msg("  [diag] ATT-weighted treatment balance computed (treated w=1, controls w=g/(1-g)).", cfg = cfg)
        if (isTRUE(cfg$diagnostics$save_csvs)) {
          write_diag_csv(bal_A_att, cfg, out_dir,
            cfg$diagnostics$balance_treatment_att_csv %||% "balance_treatment_att_loveplot_data.csv")
          all_att <- attr(bal_A_att, "all_candidate_balance_scan")
          if (!is.null(all_att) && nrow(all_att)) write_diag_csv(all_att, cfg, out_dir,
            cfg$diagnostics$balance_treatment_att_all_candidates_csv %||% "balance_treatment_att_all_candidates.csv")
          lev_att <- attr(bal_A_att, "factor_level_balance")
          if (!is.null(lev_att) && nrow(lev_att))
            write_diag_csv(lev_att, cfg, out_dir,
              cfg$diagnostics$balance_factor_levels_att_csv %||% "balance_factor_levels_treatment_att.csv")
        }
        core_bal <- build_protected_h1fs_balance(main_df, cfg, A, w, att_bal_w)
        if (nrow(core_bal) > 0L) {
          if (isTRUE(cfg$diagnostics$save_csvs))
            write_diag_csv(core_bal, cfg, out_dir,
              cfg$diagnostics$core_balance_csv %||% "protected_h1fs_att_balance.csv")
          if (any(core_bal$exceeds_0_10, na.rm = TRUE))
            warning(sprintf("%d of %d protected Wave I Feelings Scale items have post-ATT |SMD| > 0.10; inspect the protected balance audit.",
                            sum(core_bal$exceeds_0_10, na.rm = TRUE), nrow(core_bal)), call. = FALSE)
        }
        if (isTRUE(cfg$diagnostics$save_plots) && nrow(bal_A_att) > 0L) {
          plot_love(bal_A_att,
            build_unique_diag_path(cfg, out_dir, "love_plot_treatment_att.png"),
            "Treatment balance (ATT-weighted): pre vs post weighting",
            n_top = cfg$diagnostics$max_love_plot_variables %||% 30L)
        }
      }
      # ATT positivity / control-weight diagnostic: distribution of the control
      # odds weights g/(1-g) and the propensity among the treated. The ATT only
      # needs treated units to have comparable controls; extreme control weights
      # (g near 1) are the ATT-specific positivity concern.
      ctrl_odds <- (tmle_fit$gn / pmax(1 - tmle_fit$gn, 1e-6))[A == 0L]
      g_treated <- tmle_fit$gn[A == 1L]
      att_pos <- data.frame(
        metric = c("n_treated", "n_control",
                   "g_treated_min", "g_treated_median", "g_treated_max",
                   "control_oddsw_median", "control_oddsw_p95",
                   "control_oddsw_p99", "control_oddsw_max",
                   "control_oddsw_frac_gt_10", "control_oddsw_frac_gt_50"),
        value = c(sum(A == 1L), sum(A == 0L),
                  min(g_treated), stats::median(g_treated), max(g_treated),
                  stats::median(ctrl_odds),
                  stats::quantile(ctrl_odds, 0.95, names = FALSE, type = 8),
                  stats::quantile(ctrl_odds, 0.99, names = FALSE, type = 8),
                  max(ctrl_odds),
                  mean(ctrl_odds > 10), mean(ctrl_odds > 50)),
        stringsAsFactors = FALSE)
      msg(sprintf("  [diag] ATT control odds-weights: median %.2f, p99 %.2f, max %.2f; %.1f%% exceed 10.",
                  stats::median(ctrl_odds), stats::quantile(ctrl_odds, 0.99, names = FALSE, type = 8),
                  max(ctrl_odds), 100 * mean(ctrl_odds > 10)), cfg = cfg)
      if (isTRUE(cfg$diagnostics$save_csvs)) {
        write_diag_csv(att_pos, cfg, out_dir,
          cfg$diagnostics$att_positivity_csv %||% "att_positivity_control_weights.csv")
      }
    }
    pi_for_delta <- pmin(pmax(tmle_fit$pi_AW, cfg$final_tmle$pi_lower), cfg$final_tmle$pi_upper)
    censor_w <- w * ifelse(delta_Y == 1L, 1 / pi_for_delta, 1 / pmax(1 - pi_for_delta, 1e-6))
    if (length(unique(delta_Y)) == 2L) {
      bal_D <- make_balance_table(main_df, cfg, delta_Y, censor_w, "delta_Y=1_vs_delta_Y=0",
                                  priority_vars = balance_priority)
      record_balance_scan(bal_D, "delta_Y=1_vs_delta_Y=0", nrow(main_df))
      bal_D <- annotate_balance_for_love_plot(bal_D, love_plot_excluded_vars)
      if (isTRUE(cfg$diagnostics$save_csvs)) {
        write_diag_csv(bal_D, cfg, out_dir, cfg$diagnostics$balance_missingness_csv %||% "balance_missingness_loveplot_data.csv")
        lev_D <- attr(bal_D, "factor_level_balance")
        if (!is.null(lev_D) && nrow(lev_D))
          write_diag_csv(lev_D, cfg, out_dir,
            cfg$diagnostics$balance_factor_levels_missingness_csv %||% "balance_factor_levels_missingness.csv")
      }
      if (isTRUE(cfg$diagnostics$save_plots) && nrow(bal_D) > 0L) {
        plot_love(bal_D, build_unique_diag_path(cfg, out_dir, "love_plot_missingness.png"), "Outcome-observation balance: pre vs post censoring weighting", n_top = cfg$diagnostics$max_love_plot_variables %||% 30L)
      }
    }
  }

  # ---- Positivity / overlap diagnostics ------------------------------------
  if (!is.null(tmle_fit)) {
    g <- tmle_fit$gn
    pos <- data.frame(
      metric = c("g<0.05", "g>0.95", "g<=g_lower", "g>=g_upper", "g*pi1<threshold", "(1-g)*pi0<threshold"),
      fraction = c(mean(g < 0.05), mean(g > 0.95), mean(g <= cfg$final_tmle$g_lower + 1e-12),
                   mean(g >= cfg$final_tmle$g_upper - 1e-12),
                   mean(g * tmle_fit$pi_1W < cfg$final_tmle$positivity_warning_threshold),
                   mean((1 - g) * tmle_fit$pi_0W < cfg$final_tmle$positivity_warning_threshold)),
      count = c(sum(g < 0.05), sum(g > 0.95), sum(g <= cfg$final_tmle$g_lower + 1e-12),
                sum(g >= cfg$final_tmle$g_upper - 1e-12),
                sum(g * tmle_fit$pi_1W < cfg$final_tmle$positivity_warning_threshold),
                sum((1 - g) * tmle_fit$pi_0W < cfg$final_tmle$positivity_warning_threshold)),
      stringsAsFactors = FALSE)
    if (isTRUE(cfg$diagnostics$save_csvs)) {
      write_diag_csv(pos, cfg, out_dir, cfg$diagnostics$positivity_summary_csv %||% "positivity_summary.csv")
    }
    if (isTRUE(cfg$diagnostics$plot_propensity)) {
      plot_path <- build_unique_diag_path(cfg, out_dir, "propensity_by_exposure.png")
      png(plot_path, width = 1000, height = 700, res = 150)
      par(mar = c(4, 4, 2, 1))
      hist(g[A == 0L], breaks = 30, main = "Propensity by exposure", xlab = "Predicted P(A = 1 | W)", xlim = c(0, 1))
      hist(g[A == 1L], breaks = 30, add = TRUE)
      abline(v = c(cfg$final_tmle$g_lower, cfg$final_tmle$g_upper, 0.05, 0.95), lty = c(1, 1, 2, 2))
      legend("topright", legend = c("A=0", "A=1", "truncation/near-violation lines"), bty = "n")
      dev.off()
    }
  }

  # ---- Cluster-level influence diagnostics ---------------------------------
  if (!is.null(tmle_fit) && !is.null(tmle_fit$cluster_eic)) {
    ce <- tmle_fit$cluster_eic
    cl_names <- names(ce)
    cluster_chr <- as.character(cluster)
    cl_names_chr <- as.character(cl_names)
    cluster_summary <- data.frame(
      cluster = names(table(cluster_chr)),
      cluster_size = as.integer(table(cluster_chr)),
      treated_count = as.integer(tapply(A == 1L, cluster_chr, sum)),
      outcome_observed_count = as.integer(tapply(delta_Y == 1L, cluster_chr, sum)),
      weight_sum = as.numeric(tapply(w, cluster_chr, sum, na.rm = TRUE)),
      stringsAsFactors = FALSE)
    cl_diag <- data.frame(
      cluster = cl_names_chr,
      cluster_eic = as.numeric(ce),
      abs_cluster_eic = abs(as.numeric(ce)),
      stringsAsFactors = FALSE)
    cl_diag <- merge(cl_diag, cluster_summary, by = "cluster", all.x = TRUE, sort = FALSE)
    if (anyNA(cl_diag$cluster_size)) {
      bad <- cl_diag$cluster[is.na(cl_diag$cluster_size)]
      warning(sprintf("Cluster influence diagnostics: %d cluster(s) in cluster_eic were not found in the analytic data: %s",
                      length(bad), paste(head(bad, 10L), collapse = ", ")))
    }
    denom <- sum(cl_diag$cluster_eic^2)
    cl_diag$variance_share <- if (denom > 0) cl_diag$cluster_eic^2 / denom else NA_real_
    cl_diag <- cl_diag[order(-cl_diag$abs_cluster_eic), , drop = FALSE]
    if (isTRUE(cfg$diagnostics$save_csvs)) {
      write_diag_csv(head(cl_diag, 10L), cfg, out_dir, cfg$diagnostics$top_cluster_influence_csv %||% "top_cluster_influence.csv")
      write_diag_csv(cl_diag, cfg, out_dir, "cluster_influence_all.csv")
      if (!is.null(tmle_fit$D_orig) && !is.null(tmle_fit$weights)) {
        top_abs <- head(cl_diag$cluster, 5L)
        leave_rows <- do.call(rbind, lapply(c(1L, 3L, 5L), function(kdrop) {
          drop_cl <- head(top_abs, kdrop)
          keep <- !(as.character(cluster) %in% drop_cl)
          if (!any(keep)) return(NULL)
          ww <- tmle_fit$weights[keep]
          psi <- sum(ww * (tmle_fit$Qstar1W_orig[keep] - tmle_fit$Qstar0W_orig[keep]), na.rm = TRUE) / sum(ww, na.rm = TRUE)
          # Reference is the full-sample ATE (ate_tmle), since this leave-out
          # plug-in is computed over all remaining rows (untrimmed). Falls back
          # to estimate if ate_tmle is absent (older result objects).
          ref_full <- if (!is.null(tmle_fit$result$ate_tmle)) tmle_fit$result$ate_tmle[1] else tmle_fit$result$estimate[1]
          data.frame(drop_top_abs_clusters = kdrop, dropped_clusters = paste(drop_cl, collapse = ";"),
                     n_remaining = sum(keep), estimate_plugin_targeted_remaining = psi,
                     difference_from_full_ate = psi - ref_full,
                     stringsAsFactors = FALSE)
        }))
        if (!is.null(leave_rows) && nrow(leave_rows) > 0L)
          write_diag_csv(leave_rows, cfg, out_dir, cfg$diagnostics$cluster_influence_leaveout_csv %||% "cluster_influence_leaveout.csv")
      }
    }
    if (isTRUE(cfg$diagnostics$plot_qq_eic)) {
      plot_path <- build_unique_diag_path(cfg, out_dir, "cluster_eic_qq.png")
      png(plot_path, width = 900, height = 900, res = 150)
      par(mar = c(4, 4, 2, 1))
      qqnorm(tmle_fit$cluster_eic, main = "QQ plot: cluster-level EIC sums")
      qqline(tmle_fit$cluster_eic)
      dev.off()
    }
  }

  # ---- Overlap product plot -------------------------------------------------
  if (!is.null(tmle_fit) && isTRUE(cfg$diagnostics$plot_overlap_product)) {
    op1 <- tmle_fit$gn * tmle_fit$pi_1W
    op0 <- (1 - tmle_fit$gn) * tmle_fit$pi_0W
    thr <- cfg$final_tmle$positivity_warning_threshold
    plot_path <- build_unique_diag_path(cfg, out_dir, "overlap_product.png")
    png(plot_path, width = 1000, height = 700, res = 150)
    par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
    hist(op1, breaks = 30, main = "g(W) * pi(1, W)", xlab = "Product")
    abline(v = thr, lty = 2)
    hist(op0, breaks = 30, main = "(1 - g(W)) * pi(0, W)", xlab = "Product")
    abline(v = thr, lty = 2)
    dev.off()
  }

  # ---- Fold time and outcome distribution ----------------------------------
  if (!is.null(tmle_fit) && isTRUE(cfg$diagnostics$plot_fold_times)) {
    plot_path <- build_unique_diag_path(cfg, out_dir, "fold_times.png")
    png(plot_path, width = 900, height = 600, res = 150)
    par(mar = c(4, 4, 2, 1))
    barplot(tmle_fit$fold_times, names.arg = seq_along(tmle_fit$fold_times),
            xlab = "Outer fold", ylab = "Seconds", main = "Final CV-TMLE per-fold runtime")
    dev.off()
  }
  if (isTRUE(cfg$diagnostics$plot_outcome_distribution)) {
    y <- main_df[[cfg$analysis$outcome_var]]
    y_ok <- y[is.finite(y)]
    if (length(y_ok) > 0L) {
      plot_path <- build_unique_diag_path(cfg, out_dir, "outcome_distribution.png")
      png(plot_path, width = 900, height = 700, res = 150)
      par(mar = c(4, 4, 2, 1))
      hist(y_ok, breaks = 40, main = "Outcome distribution", xlab = cfg$analysis$outcome_var)
      dev.off()
    }
  }

  # ---- Missing-not-at-random pattern-mixture sensitivity --------------------
  if (!is.null(tmle_fit) && isTRUE(cfg$diagnostics$save_csvs)) {
    if (isTRUE(cfg$diagnostics$enable_mnar_pattern_mixture %||% TRUE)) {
      mnar <- build_att_mnar_pattern_mixture(tmle_fit, cfg)
      if (nrow(mnar))
        write_diag_csv(mnar, cfg, out_dir,
                       cfg$diagnostics$mnar_pattern_mixture_csv %||%
                         "att_mnar_pattern_mixture.csv")
    }
    # --- MNAR sensitivity extensions (v8.28) --------------------------------
    # Fixed-nuisance, no refit, estimand unchanged.  Each is wrapped in
    # run_optional() so that a failure here can never abort a completed run.
    if (isTRUE(cfg$diagnostics$enable_mnar_breakdown %||% TRUE)) {
      run_optional("att MNAR breakdown point", function() {
        z <- build_att_mnar_breakdown(tmle_fit, cfg)
        if (nrow(z))
          write_diag_csv(z, cfg, out_dir,
                         cfg$diagnostics$mnar_breakdown_csv %||%
                           "att_mnar_breakdown_point.csv")
        invisible(NULL)
      })
    }
    if (isTRUE(cfg$diagnostics$enable_manski_bounds %||% TRUE)) {
      run_optional("att fixed-nuisance extreme-mean bounds", function() {
        z <- build_att_manski_bounds(tmle_fit, cfg)
        if (nrow(z))
          write_diag_csv(z, cfg, out_dir,
                         cfg$diagnostics$manski_bounds_csv %||%
                           "att_fixed_nuisance_extreme_mean_bounds.csv")
        invisible(NULL)
      })
    }
    if (isTRUE(cfg$diagnostics$enable_mnar_calibrated %||% TRUE)) {
      run_optional("att MNAR calibrated sensitivity", function() {
        z <- build_att_mnar_calibrated(tmle_fit, cfg)
        if (nrow(z))
          write_diag_csv(z, cfg, out_dir,
                         cfg$diagnostics$mnar_calibrated_csv %||%
                           "att_mnar_calibrated_sensitivity.csv")
        invisible(NULL)
      })
    }
  }

  # ---- Approximate E-value --------------------------------------------------
  if (!is.null(tmle_fit) && isTRUE(cfg$diagnostics$save_csvs) &&
      isTRUE(cfg$diagnostics$enable_evalue %||% FALSE)) {
    ac <- tmle_fit$att_components
    if (is.null(ac)) stop("E-value diagnostic requires ATT components.", call. = FALSE)
    evalue_weights <- ac$weights * ac$A * ac$delta_Y / ac$pi_AW
    ev <- make_evalue_approx(tmle_fit$result$estimate[1], tmle_fit$result$ci_lower[1],
                             tmle_fit$result$ci_upper[1], ac$Y_bounded_orig,
                             weights = evalue_weights, baseline_mean = ac$mu0_att)
    write_diag_csv(ev, cfg, out_dir, cfg$diagnostics$evalue_csv %||% "evalue_sensitivity.csv")
    ev_contour <- make_evalue_contour(ev$approx_rr[1])
    write_diag_csv(ev_contour, cfg, out_dir,
                   cfg$diagnostics$evalue_contour_csv %||% "evalue_contour.csv")
  }

  if (isTRUE(cfg$diagnostics$enable_wave2_completion_diagnostic %||% TRUE) &&
      !is.null(tmle_fit) && !is.null(w1_all)) {
    w2x <- run_optional("expanded_wave2_completion", function() {
      ans <- run_expanded_wave2_completion_diagnostic(
        cfg, main_df, tmle_fit, w1_all)
      if (!is.null(ans) && isTRUE(cfg$diagnostics$save_csvs)) {
        write_diag_csv(
          ans$balance, cfg, out_dir,
          cfg$diagnostics$wave2_completion_expanded_balance_csv %||%
            "wave2_completion_expanded_balance.csv")
        w2_all <- attr(ans$balance, "all_candidate_balance_scan")
        if (!is.null(w2_all) && nrow(w2_all))
          write_diag_csv(
            w2_all, cfg, out_dir,
            cfg$diagnostics$wave2_completion_all_candidates_csv %||%
              "wave2_completion_all_candidates.csv")
        write_diag_csv(
          ans$model, cfg, out_dir,
          cfg$diagnostics$wave2_completion_model_csv %||%
            "wave2_completion_model.csv")
      }
      ans
    })
    if (!is.null(w2x))
      record_balance_scan(
        w2x$balance, "Wave2_CESD_complete_vs_incomplete_expanded",
        w2x$model$n[1L] %||% NA_integer_)
  }

  if (length(balance_scan_timing_rows) && isTRUE(cfg$diagnostics$save_csvs))
    write_diag_csv(
      do.call(rbind, balance_scan_timing_rows), cfg, out_dir,
      cfg$diagnostics$balance_scan_timing_csv %||%
        "balance_scan_timings.csv")
  if (length(optional_status_rows) && isTRUE(cfg$diagnostics$save_csvs))
    write_diag_csv(
      do.call(rbind, optional_status_rows), cfg, out_dir,
      cfg$diagnostics$diagnostic_status_csv %||% "diagnostic_status.csv")

  msg("===== Diagnostics complete. =====\n", cfg = cfg)
  invisible(sample_flow)
}


# =============================================================================
